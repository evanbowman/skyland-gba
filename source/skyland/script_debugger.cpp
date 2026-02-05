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



namespace skyland
{



namespace
{

constexpr auto bkg_color = custom_color(0x007cbf);

static constexpr const Text::OptColors text_colors{
    {custom_color(0xffffff), bkg_color}};

static constexpr const Text::OptColors text_colors_inv{
    {text_colors->background_, text_colors->foreground_}};


}



static EXT_WORKRAM_DATA enum class DisplayTab : u8 {
    callstack,
    local_vars,
    operand_stack,
    arguments,
    count
} debug_display_tab;



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


static void onscreen_debugger_render_tab(u32& scroll)
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 6; y < 20; ++y) {
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
            Text::print(format("% %",
                               i,
                               lisp::val_to_string<30>(arg).c_str()).c_str(),
                        {1, y},
                        text_colors);
        }
        break;
    }

    case DisplayTab::count:
        break;
    }
}



lisp::debug::Action handle_debug_step(lisp::Value* expr)
{
    if (not lisp::is_list(expr) or length(expr) == 0) {
        // Only halt on expressions, not atoms
        return lisp::debug::Action::step;
    }

    PLATFORM.screen().schedule_fade(1, Platform::Screen::FadeProperties{
            .color = bkg_color
        });

    PLATFORM.set_overlay_origin(0, 0);
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();

    StringBuffer<33> fmt_str;
    fmt_str = lisp::val_to_string<32>(expr);
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

    u32 scroll = 0;
    onscreen_debugger_render_tab(scroll);

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

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(scroll);
        }

        if (button_down<Button::left>()) {
            scroll = 0;
            if (debug_display_tab == (DisplayTab)0) {
                debug_display_tab = (DisplayTab)((int)DisplayTab::count - 1);
            } else {
                debug_display_tab = (DisplayTab)((int)debug_display_tab - 1);
            }
            onscreen_debugger_render_tab(scroll);
        } else if (button_down<Button::right>()) {
            scroll = 0;
            if ((int)debug_display_tab < (int)DisplayTab::count - 1) {
                debug_display_tab = (DisplayTab)((int)debug_display_tab + 1);
            } else {
                debug_display_tab = (DisplayTab)0;
            }
            onscreen_debugger_render_tab(scroll);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);

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
            button_down<Button::action_2>()) {
            break;
        }

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(scroll);
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
            button_down<Button::action_2>()) {
            break;
        }

        if (button_down<Button::down>()) {
            ++scroll;
            onscreen_debugger_render_tab(scroll);
        }
        if (button_down<Button::up>() and scroll > 0) {
            --scroll;
            onscreen_debugger_render_tab(scroll);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }

    PLATFORM.fill_overlay(0);
    PLATFORM.screen().schedule_fade(0);
}



lisp::debug::Action onscreen_script_debug_handler(lisp::debug::Interrupt irq,
                                                  lisp::Value* expr)
{
    switch (irq) {
    case lisp::debug::Interrupt::step:
        return handle_debug_step(expr);

    case lisp::debug::Interrupt::breakpoint:
        handle_breakpoint(expr);
        break;

    case lisp::debug::Interrupt::watchpoint:
        handle_watchpoint(expr);
        break;
    }

    return lisp::debug::Action::resume;
}


}
