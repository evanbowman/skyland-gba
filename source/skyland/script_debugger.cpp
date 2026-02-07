////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "script_debugger.hpp"
#include "graphics/overlay.hpp"
#include "ext_workram_data.hpp"



void print_char(utf8::Codepoint c,
                const OverlayCoord& coord,
                const Optional<FontColors>& colors);



namespace skyland
{



namespace
{

constexpr auto bkg_color = custom_color(0x007cbf);

static constexpr const Text::OptColors text_colors{
    {custom_color(0xffffff), bkg_color}};

static constexpr const Text::OptColors text_colors_inv{
    {text_colors->background_, text_colors->foreground_}};

static constexpr const Text::OptColors text_colors_highlight{
    {custom_color(0x00508a), custom_color(0xf9e965)}};


}



static EXT_WORKRAM_DATA enum class DisplayTab : u8 {
    codeview,
    callstack,
    local_vars,
    operand_stack,
    arguments,
    count
} debug_display_tab;


static EXT_WORKRAM_DATA bool debugger_active;



static auto get_callstack()
{
    lisp::Protected strace(lisp::stacktrace());
    int stack_depth = lisp::length(strace);
    Buffer<lisp::Value*, 32> frames;
    frames.push_back(lisp::get_this());
    for (int i = 0; i < stack_depth; ++i) {
        frames.push_back(lisp::get_list(strace, i));
    }
    return frames;
}



void show_callstack(u8 start_y, u32& scroll)
{
    auto frames = get_callstack();
    scroll = clamp((int)scroll, 0, (int)frames.size() - 1);
    for (int i = scroll; i < (int)frames.size() and start_y + (i - scroll) * 2 < 20; ++i) {
        auto fn = frames[i];
        u8 y = (start_y + (i - scroll) * 2);
        Text::print(format("%", i).c_str(), {1, y}, text_colors);
        Text::print(lisp::val_to_string<30>(fn).c_str(),
                    OverlayCoord{2, y},
                    text_colors);
    }
}



struct PrinterState
{
    Vector<char> output_;
    lisp::Value* match_expr_;
    int match_begin_line_ = -1;
    int match_end_line_ = -1;
    int match_begin_offset_ = -1;
    int match_end_offset_ = -1;
    int match_begin_abs_offset_ = -1;
    int depth_ = 0;
    int linecount_ = 0;
    int chars_since_newline_ = 0;
};


void pretty_print_newline(PrinterState& ps)
{
    ps.chars_since_newline_ = 0;
    ps.output_.push_back('\n');
    for (int i = 0; i < ps.depth_ * 2; ++i) {
        ps.output_.push_back(' ');
        ++ps.chars_since_newline_;
    }
    ++ps.linecount_;
}


void pretty_print_append(PrinterState& ps, const char* str)
{
    while (*str not_eq '\0') {
        ps.output_.push_back(*str);
        ++ps.chars_since_newline_;
        ++str;
    }
}



void pretty_print_atom(PrinterState& ps, lisp::Value* atom)
{
    auto p = allocate<lisp::DefaultPrinter>("...");
    if (atom->type() == lisp::Value::Type::cons and
        atom->cons().car()->type() == lisp::Value::Type::symbol and
        str_eq("'", atom->cons().car()->symbol().name())) {
        pretty_print_append(ps, "'");
        lisp::format(atom->cons().cdr(), *p);
    } else if (atom->type() == lisp::Value::Type::cons and
        atom->cons().car()->type() == lisp::Value::Type::symbol and
        str_eq("`", atom->cons().car()->symbol().name())) {
        pretty_print_append(ps, "`");
        lisp::format(atom->cons().cdr(), *p);
    } else {
        lisp::format(atom, *p);
    }

    if (p->data_ == "(')") {
        pretty_print_append(ps, "'()");
    } else {
        pretty_print_append(ps, p->data_.c_str());
    }
}



void pretty_print_impl(PrinterState& ps,
                       lisp::Value* current_expr)
{
    if (lisp::is_list(current_expr) and length(current_expr) == 0) {
        pretty_print_append(ps, "'()");
    } else if (lisp::is_list(current_expr)) {
        if (current_expr == ps.match_expr_) {
            ps.match_begin_line_ = ps.linecount_;
            for (int i = ps.output_.size() - 1; i > -1; --i) {
                if (ps.output_[i] == '\n') {
                    break;
                }
                ++ps.match_begin_offset_;
            }
            ps.match_begin_abs_offset_ = ps.output_.size();
        }

        ps.output_.push_back('(');

        if (current_expr->cons().car()->type() == lisp::Value::Type::symbol) {
            auto sym = current_expr->cons().car();
            if (str_eq(sym->symbol().name(), "if") or
                str_eq(sym->symbol().name(), "while")) {
                pretty_print_append(ps, sym->symbol().name());
                pretty_print_append(ps, " ");
                auto lat = current_expr->cons().cdr();
                bool printed_cond = false;
                while (lat not_eq L_NIL) {
                    pretty_print_impl(ps, lat->cons().car());
                    lat = lat->cons().cdr();
                    if (not printed_cond) {
                        printed_cond = true;
                        ++ps.depth_;
                    }
                    if (lat not_eq L_NIL) {
                        pretty_print_newline(ps);
                    } else {
                        ps.output_.push_back(')');
                    }
                }
                if (printed_cond) {
                    --ps.depth_;
                }
            } else if (str_eq(sym->symbol().name(), "fn")) {
                pretty_print_append(ps, "fn ");
                ++ps.depth_;
                pretty_print_newline(ps);
                auto lat = current_expr->cons().cdr();
                while (lat not_eq L_NIL) {
                    pretty_print_impl(ps, lat->cons().car());
                    lat = lat->cons().cdr();
                    if (lat not_eq L_NIL) {
                        pretty_print_newline(ps);
                    } else {
                        ps.output_.push_back(')');
                    }
                }
                --ps.depth_;
            } else if (str_eq(sym->symbol().name(), "let")) {
                pretty_print_append(ps, sym->symbol().name());
                pretty_print_append(ps, " ");
                pretty_print_impl(ps, lisp::get_list(current_expr, 1));
                ++ps.depth_;
                if (lisp::get_list(current_expr, 2) not_eq L_NIL) {
                    pretty_print_newline(ps);
                    auto lat = current_expr->cons().cdr()->cons().cdr();
                    while (lat not_eq L_NIL) {
                        pretty_print_impl(ps, lat->cons().car());
                        lat = lat->cons().cdr();
                        if (lat not_eq L_NIL) {
                            pretty_print_newline(ps);
                        } else {
                            ps.output_.push_back(')');
                        }
                    }
                }
                --ps.depth_;
            } else {
                auto lat = current_expr;
                bool first = true;
                while (lat not_eq L_NIL) {
                    if (not first) {
                        pretty_print_append(ps, " ");
                    }
                    pretty_print_impl(ps, lat->cons().car());
                    lat = lat->cons().cdr();
                    first = false;
                }
                ps.output_.push_back(')');
            }
        } else {
            auto lat = current_expr;
                bool first = true;
                while (lat not_eq L_NIL) {
                    if (not first) {
                        pretty_print_append(ps, " ");
                    }
                    pretty_print_impl(ps, lat->cons().car());
                    lat = lat->cons().cdr();
                    first = false;
                }
                ps.output_.push_back(')');
        }

        if (current_expr == ps.match_expr_) {
            ps.match_end_line_ = ps.linecount_;
            ps.match_end_offset_ =
                ps.match_begin_offset_ + (ps.output_.size() - ps.match_begin_abs_offset_);
        }
    } else {
        pretty_print_atom(ps, current_expr);
    }
}



void pretty_print_begin(PrinterState& ps, lisp::Value* current_expr)
{
    if (lisp::is_list(current_expr)) {
        while (current_expr not_eq L_NIL) {
            pretty_print_impl(ps, current_expr->cons().car());
            pretty_print_newline(ps);
            current_expr = current_expr->cons().cdr();
        }
    }
}



static int calculate_skip_lines(const PrinterState& ps, int visible_lines = 6)
{
    int center_offset = visible_lines / 2;  // 3 for 6 lines

    // Try to center the match
    int skip = ps.match_begin_line_ - center_offset;

    // Don't scroll past the beginning
    if (skip < 0) {
        skip = 0;
    }

    // Don't scroll past the end
    int max_skip = ps.linecount_ - visible_lines;
    if (max_skip < 0) {
        max_skip = 0;  // Function shorter than viewport
    }
    if (skip > max_skip) {
        skip = max_skip;
    }

    return skip;
}



void pretty_print_current_fn_with_expr(lisp::Value* expr)
{
    auto current_fn = lisp::get_this();
    if (current_fn->type() not_eq lisp::Value::Type::function) {
        return;
    }
    const auto required_fn_type = lisp::Function::ModeBits::lisp_function;
    if (current_fn->hdr_.mode_bits_ not_eq required_fn_type) {
        return;
    }

    auto fn_impl = lisp::dcompr(current_fn->function().lisp_impl_.code_);
    PrinterState ps;
    ps.match_expr_ = expr;
    pretty_print_begin(ps, fn_impl);

    auto skip_lines = calculate_skip_lines(ps);

    StringBuffer<30> line;
    u8 y = 8;
    [[maybe_unused]] u32 line_start = 0;
    int linum = 0;
    for (u32 i = 0; i < ps.output_.size(); ++i) {
        if (ps.output_[i] == '\n') {
            if (linum < skip_lines) {

            } else {
                auto colors = text_colors;

                u8 write_pos = 1;
                int len = 0;
                utf8::scan([&](const utf8::Codepoint& cp, const char* raw, int) {
                    if (linum >= ps.match_begin_line_ and
                        linum <= ps.match_end_line_) {
                        if (len > ps.match_end_offset_) {
                            colors = text_colors;
                        } else if (len > ps.match_begin_offset_ and linum == ps.match_begin_line_) {
                            colors = text_colors_highlight;
                        } else if (linum > ps.match_begin_line_) {
                            colors = text_colors_highlight;
                        }
                    }
                    print_char(cp, {write_pos, y}, colors);
                    ++write_pos;
                    ++len;
                    return true;
                },
                    line.c_str(),
                    strlen(line.c_str()));

                // Text::print(line.c_str(), {1, y}, colors);

                y += 2;
                if (y >= 20) {
                    break;
                }
            }

            line.clear();
            line_start = i;
            ++linum;
        } else {
            line.push_back(ps.output_[i]);
        }
    }
}



static void onscreen_debugger_render_tab(lisp::Value* expr, u32& scroll)
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 6; y < 19; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    switch (debug_display_tab) {
    case DisplayTab::callstack: {
        Text::print("<- callstack ->", {1, 6}, text_colors_inv);
        show_callstack(8, scroll);
        break;
    }

    case DisplayTab::local_vars: {
        Text::print("<- local vars ->", {1, 6}, text_colors_inv);
        auto locals = lisp::debug::get_locals();
        int start_y = 8;
        scroll = clamp((int)scroll, 0, (int)locals.size() - 1);
        for (u32 i = scroll; i < locals.size() and (i - scroll) * 2 + start_y < 20; ++i) {
            u8 y = (start_y + i * 2);
            StringBuffer<30> out;
            out += locals[i].name_;
            out += ": ";
            out += lisp::val_to_string<30>(locals[i].value_);
            Text::print(out.c_str(), OverlayCoord{1, y}, text_colors);
        }
        break;
    }

    case DisplayTab::operand_stack: {
        Text::print("<- value stack ->", {1, 6}, text_colors_inv);
        int start_y = 8;
        scroll = clamp((int)scroll, 0, (int)lisp::get_op_count() - 1);
        for (u32 i = scroll; i < lisp::get_op_count() and (i - scroll) * 2 + start_y < 20; ++i) {
            u8 y = start_y + (i - scroll) * 2;
            StringBuffer<30> out;
            out += stringify(i);
            out += ": ";
            out += lisp::val_to_string<30>(lisp::get_op(i));
            Text::print(out.c_str(), OverlayCoord{1, y}, text_colors);
        }
        break;
    }

    case DisplayTab::arguments: {
        Text::print("<- arguments ->", {1, 6}, text_colors_inv);
        int start_y = 8;
        for (u32 i = 0; i < lisp::get_argc() and i * 2 + start_y < 20; ++i) {
            auto arg = lisp::get_arg(i);
            u8 y = (start_y + i * 2);
            Text::print(format("$% %",
                               i,
                               lisp::val_to_string<28>(arg).c_str()).c_str(),
                        {1, y},
                        text_colors);
        }
        break;
    }

    case DisplayTab::codeview: {
        StringBuffer<18> name = "?";
        auto current_fn = lisp::get_this();
        if (current_fn->type() == lisp::Value::Type::function) {
            if (auto repr_name = lisp::nameof(current_fn)) {
                name = repr_name;
            }
        }
        Text::print(format("<- code: % ->", name.c_str()).c_str(),
                    {1, 6}, text_colors_inv);
        pretty_print_current_fn_with_expr(expr);
        break;
    }

    case DisplayTab::count:
        break;
    }
}



static void print_heap_usage()
{
    StringBuffer<30> mem_used_str = "mem:";
    mem_used_str += stringify(lisp::value_pool_info().first);
    Text::print(mem_used_str.c_str(),
                {(u8)(30 - mem_used_str.length()), 0},
                text_colors);
}



lisp::debug::Action handle_debug_step(lisp::Value* expr)
{
    if (not lisp::is_list(expr) or length(expr) == 0) {
        // Only halt on expressions, not atoms
        return lisp::debug::Action::step;
    }

    enable_text_icon_glyphs(false);

    PLATFORM.screen().schedule_fade(1, Platform::Screen::FadeProperties{
            .color = bkg_color
        });

    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();


    print_heap_usage();

    StringBuffer<30> fmt_str;
    {
        PrinterState ps;
        ps.match_expr_ = expr;
        pretty_print_impl(ps, expr);
        for (char c : ps.output_) {
            if (c not_eq '\n') {
                fmt_str.push_back(c);
            }
        }
    }

    if (fmt_str.length() > 0 and fmt_str[0] == '\'') {
        fmt_str.erase(fmt_str.begin());
    }

    Text::print(fmt_str.c_str(),
                {1, 3},
                text_colors);

    fmt_str = "expr: ";
    Text::print(fmt_str.c_str(),
                {1, 1},
                text_colors_inv);

    Text::print(" A: into, R: over, B: continue", {0, 19}, text_colors_inv);

    u32 scroll = 0;
    onscreen_debugger_render_tab(expr, scroll);

    lisp::debug::Action resp;

    while (true) {
        PLATFORM.input().poll();
        PLATFORM_EXTENSION(feed_watchdog);
        PLATFORM.delta_clock().reset();

        if (button_down<Button::action_1>()) {
            resp = lisp::debug::Action::step;
            break;
        }
        if (button_down<Button::action_2>()) {
            resp = lisp::debug::Action::resume;
            break;
        }

        if (button_down<Button::alt_2>()) {
            resp = lisp::debug::Action::step_over;
            break;
        }

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }

        if (button_down<Button::left>()) {
            scroll = 0;
            if (debug_display_tab == (DisplayTab)0) {
                debug_display_tab = (DisplayTab)((int)DisplayTab::count - 1);
            } else {
                debug_display_tab = (DisplayTab)((int)debug_display_tab - 1);
            }
            onscreen_debugger_render_tab(expr, scroll);
        } else if (button_down<Button::right>()) {
            scroll = 0;
            if ((int)debug_display_tab < (int)DisplayTab::count - 1) {
                debug_display_tab = (DisplayTab)((int)debug_display_tab + 1);
            } else {
                debug_display_tab = (DisplayTab)0;
            }
            onscreen_debugger_render_tab(expr, scroll);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);

    enable_text_icon_glyphs(true);

    return resp;
}



void handle_breakpoint(lisp::Value* expr)
{
    PLATFORM.screen().schedule_fade(1, Platform::Screen::FadeProperties{
            .color = bkg_color
        });

    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    print_heap_usage();

    Text::print("breakpoint! symbol:", {1, 1}, text_colors_inv);
    Text::print(lisp::val_to_string<30>(expr).c_str(),
                {1, 3},
                text_colors);

    Text::print("callstack", {1, 6}, text_colors_inv);
    u32 scroll = 0;
    show_callstack(8, scroll);

    while (true) {
        PLATFORM.input().poll();
        PLATFORM_EXTENSION(feed_watchdog);
        PLATFORM.delta_clock().reset();

        if (button_down<Button::action_1>() or
            button_down<Button::action_2>() or
            button_down<Button::alt_2>()) {
            break;
        }

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);
}



void handle_watchpoint(lisp::Value* expr)
{
    PLATFORM.screen().schedule_fade(1, Platform::Screen::FadeProperties{
            .color = bkg_color
        });

    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    print_heap_usage();

    Text::print("watchpoint! symbol:", {1, 1}, text_colors_inv);
    Text::print(lisp::val_to_string<30>(expr->cons().car()).c_str(),
                {1, 3},
                text_colors);

    Text::print("updated to:", {1, 5}, text_colors_inv);

    Text::print(lisp::val_to_string<30>(expr->cons().cdr()).c_str(),
                {1, 7},
                text_colors);

    Text::print("callstack", {1, 9}, text_colors_inv);
    u32 scroll = 0;
    show_callstack(11, scroll);

    while (true) {
        PLATFORM.input().poll();
        PLATFORM_EXTENSION(feed_watchdog);
        PLATFORM.delta_clock().reset();

        if (button_down<Button::action_1>() or
            button_down<Button::action_2>() or
            button_down<Button::alt_2>()) {
            break;
        }

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(expr, scroll);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);
}



lisp::debug::Action handle_enter_compiled_function(lisp::Value* expr)
{
    PLATFORM.screen().schedule_fade(1, Platform::Screen::FadeProperties {
            .color = bkg_color
        });

    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    print_heap_usage();

    const char* category = "compiled function:";
    if (expr->type() == lisp::Value::Type::function and
        expr->hdr_.mode_bits_ == lisp::Function::ModeBits::cpp_function) {
        category = "native function:";
    }

    Text::print(category, {1, 1}, text_colors_inv);

    StringBuffer<30> fmt_str;

    if (auto name = lisp::nameof(expr)) {
        fmt_str += name;
    } else {
        fmt_str += "??";
    }

    Text::print(fmt_str.c_str(), {1, 3}, text_colors);

    Text::print("arguments:", {1, 5}, text_colors_inv);
    int start_y = 7;
    if (lisp::get_argc() == 0) {
        Text::print("none.", {1, (u8)start_y}, text_colors);
    }
    for (u32 i = 0; i < lisp::get_argc() and i * 2 + start_y < 20; ++i) {
        auto arg = lisp::get_arg(i);
        u8 y = (start_y + i * 2);
        Text::print(format("$% %",
                           i,
                           lisp::val_to_string<28>(arg).c_str()).c_str(),
                    {1, y},
                    text_colors);
    }

    auto resp = lisp::debug::Action::step;
    while (true) {
        PLATFORM.input().poll();
        PLATFORM_EXTENSION(feed_watchdog);
        PLATFORM.delta_clock().reset();

        if (button_down<Button::action_1>()) {
            break;
        }

        if (button_down<Button::action_2>()) {
            resp = lisp::debug::Action::resume;
            break;
        }

        if (button_down<Button::alt_2>()) {
            // FIXME: the lisp runtime ignores step_over for compile functions
            // and keeps stepping into them.
            resp = lisp::debug::Action::step_over;
            break;
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);

    return resp;
}



lisp::debug::Action onscreen_script_debug_handler(lisp::debug::Interrupt irq,
                                                  lisp::Value* expr)
{
    if (debugger_active) {
        return lisp::debug::Action::step;
    }

    debugger_active = true;

    auto result = lisp::debug::Action::resume;

    switch (irq) {
    case lisp::debug::Interrupt::enter_compiled_function:
        result = handle_enter_compiled_function(expr);
        break;

    case lisp::debug::Interrupt::step:
        result = handle_debug_step(expr);
        break;

    case lisp::debug::Interrupt::breakpoint:
        handle_breakpoint(expr);
        break;

    case lisp::debug::Interrupt::watchpoint:
        handle_watchpoint(expr);
        break;
    }

    debugger_active = false;

    return result;
}


}
