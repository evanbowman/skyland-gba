////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "textEditorModule.hpp"
#include "fileBrowserModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/scene/sramFileWritebackScene.hpp"
#include "skyland/skyland.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



int TextEditorModule::y_max() const
{
    if (gui_mode_) {
        return calc_screen_tiles().y - 4;
    } else {
        return calc_screen_tiles().y - 1;
    }
}



static const auto status_colors =
    FontColors{custom_color(0x007cbf), custom_color(0xffffff)};


static const auto highlight_colors =
    FontColors{custom_color(0xffffff), custom_color(0xF71735)};



u8 TextEditorModule::cursor_y_offset() const
{
    if (gui_mode_) {
        return 4;
    } else {
        return 1;
    }
}



void TextEditorModule::show_status()
{
    if (mode_ == Mode::autocomplete) {
        return;
    }

    auto colors = status_colors;

    if (mode_ == Mode::edit) {
        status_->assign("edit: (size ", colors);
        status_->append(text_buffer_.size(), colors);
        status_->append(")", colors);
    } else {
        status_->assign("nav: line ", colors);
        status_->append(cursor_.y + 1, colors);
        status_->append("/", colors);
        status_->append(line_count_ + 1, colors);
        status_->append(" col ", colors);
        status_->append(cursor_.x + 1, colors);

        if (state_->sel_begin_) {
            status_->append(" (sel)", colors);
        }
    }

    while (status_->len() not_eq calc_screen_tiles().x) {
        status_->append(" ", colors);
    }
}



static const char* keyboard[7][7] = {{"z", "y", "g", "f", "v", "q", ";"},
                                     {"m", "b", "i", "d", "l", "j", "\""},
                                     {"w", "a", "o", "e", "u", "k", "/"},
                                     {"p", "h", "t", "n", "s", "r", "_"},
                                     {"x", "c", "(", ")", "-", " ", "."},
                                     {"$", "'", "0", "1", "2", "3", "X"},
                                     {"4", "5", "6", "7", "8", "9", "\n"}};



void TextEditorModule::render_keyboard()
{
    for (int x = 0; x < 7; ++x) {
        for (int y = 0; y < 7; ++y) {
            const char c = keyboard[y][x][0];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = PLATFORM.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if (x == keyboard_cursor_.x and y == keyboard_cursor_.y) {
                colors =
                    FontColors{custom_color(0xffffff), custom_color(0x007cbf)};
            }

            u8 yo = 6;
            if (gui_mode_) {
                yo = 9;
            }

            PLATFORM.set_tile((calc_screen_tiles().x - 8) + x,
                              (u8((calc_screen_tiles().y - 1)) - yo) + y,
                              t,
                              colors);
        }
    }
}



void TextEditorModule::render_completions()
{
    char c = ' ';
    const u16 space = PLATFORM.map_glyph(c, *locale_texture_map()(c));

    int line = 13;
    for (auto& cpl : state_->completions_) {

        auto colors = status_colors;
        if (line - 13 == selected_completion_) {
            colors = FontColors{custom_color(0xffffff), custom_color(0x007cbf)};
        }

        u8 y = line;
        if (gui_mode_) {
            y -= 3;
        }

        u32 x;
        for (x = 0; x < calc_screen_tiles().x and x < cpl.length(); ++x) {
            char c = cpl[x];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = PLATFORM.map_glyph(c, *mapping_info);

            auto prefix_colors = colors;

            if (x < state_->current_word_.length() and
                line - 13 not_eq selected_completion_) {
                prefix_colors.foreground_ = custom_color(0xffffff);
            }

            PLATFORM.set_tile(x, y, t, prefix_colors);
        }

        for (; x < calc_screen_tiles().x; ++x) {
            PLATFORM.set_tile(x, y, space, colors);
        }

        ++line;
    }

    for (; line < calc_screen_tiles().y; ++line) {
        for (int x = 0; x < calc_screen_tiles().x; ++x) {
            u8 y = line;
            if (gui_mode_) {
                y -= 3;
            }
            PLATFORM.set_tile(x, y, space, status_colors);
        }
    }
}



void TextEditorModule::handle_char(Vector<Glyph>::Iterator data,
                                   utf8::Codepoint c,
                                   ParserState& ps)
{
    ps.parse_word_.clear();

    if (syntax_mode_ == SyntaxMode::plain_text) {
        return;
    } else if (syntax_mode_ == SyntaxMode::python) {

        bool start_of_line = false;
        if (data == text_buffer_.begin()) {
            start_of_line = true;
        } else {
            auto iter_cpy = data;
            --iter_cpy;
            if (*iter_cpy == newline_glyph_) {
                start_of_line = true;
            }
        }

        if (c == '#') {
            ps.comment = true;
        } else if (c == '"') {
            if (not ps.quotation) {
                ps.quotation = true;
            } else {
                ps.endquote = true;
            }
        } else if (c == '\n' or c == ' ' or c == ')' or c == '(' or
                   start_of_line) {
            ps.keyword = false;

            auto seek = data;
            if (not start_of_line) {
                ++seek;
            }

            auto& word = ps.parse_word_;

            auto seek_cp = seek->cp(*this);
            while (seek_cp not_eq '\0' and seek_cp not_eq ' ' and
                   seek_cp not_eq '(' and seek_cp not_eq ')' and
                   seek_cp not_eq '\n') {
                word.push_back(seek_cp);
                ++seek;
                seek_cp = seek->cp(*this);
            }

            if (word.empty()) {
                return;
            }

            if (word == "if" or word == "with" or word == "import" or
                word == "def" or word == "elif" or word == "else" or
                word == "for" or word == "as" or word == "in") {
                ps.keyword = true;
            }
        }
    } else if (syntax_mode_ == SyntaxMode::ini) {
        if (c == '#') {
            ps.comment = true;
        } else if (c == '[') {
            ps.quotation = true;
        } else if (c == ']') {
            ps.endquote = true;
        }
    } else if (syntax_mode_ == SyntaxMode::lisp) {
        if (c == ';') {
            ps.comment = true;
        } else if (c == '"') {
            if (not ps.quotation) {
                ps.quotation = true;
            } else {
                ps.endquote = true;
            }
        } else if (c == '\n' or c == ' ' or c == ')' or c == '(') {
            ps.keyword = false;

            auto seek = data;
            ++seek;

            auto& word = ps.parse_word_;

            auto seek_cp = seek->cp(*this);
            while (seek_cp not_eq '\0' and seek_cp not_eq ' ' and
                   seek_cp not_eq '(' and seek_cp not_eq ')' and
                   seek_cp not_eq '\n') {
                word.push_back(seek_cp);
                ++seek;
                seek_cp = seek->cp(*this);
            }

            if (word.empty()) {
                return;
            }

            if (word == "setq" or word == "defn/c" or word == "defn" or
                word == "let" or word == "lambda" or word == "if" or
                word == "or" or word == "and" or word == "cond" or
                word == "progn" or word[0] == '$') {
                ps.keyword = true;
            }
        }
    }
}



template <typename F>
void parse_words(TextEditorModule& m,
                 Vector<TextEditorModule::Glyph>::Iterator data,
                 F&& callback)
{
    TextEditorModule::ParserState ps;

    while (data->cp(m) not_eq '\0') {

        ps.endquote = false;

        if (data->cp(m) == '\n') {
            ps = TextEditorModule::ParserState{};
        } else {
            m.handle_char(data, data->cp(m), ps);
        }

        if (ps.endquote) {
            ps.quotation = false;
        }

        if (not ps.parse_word_.empty() and not ps.comment and
            not ps.quotation) {
            callback(ps.parse_word_);
        }

        ++data;
    }
}



void TextEditorModule::render(int start_line)
{
    int x = 0;
    int y = 1;
    if (gui_mode_) {
        y = 4;
    }

    auto data = text_buffer_.begin();

    while (start_line) {
        if (*data == null_glyph_) {
            break;
        }

        if (*data == newline_glyph_) {
            --start_line;
        }

        ++data;
    }

    int skipped = 0;

    ParserState ps;


    while (data->cp(*this) not_eq '\0' and y not_eq y_max()) {

        ps.endquote = false;

        auto on_newline = [&] {
            x = 0;
            skipped = 0;
            ps.comment = false;
            ps.quotation = false;
        };

        if (x == calc_screen_tiles().x) {
            while (*data not_eq newline_glyph_) {
                if (*data == null_glyph_) {
                    goto FILL;
                }
                ++data;
            }
            ++data;
            ++y;
            on_newline();
            continue;
        }

        bool within_sel = false;
        if (state_->sel_begin_) {
            within_sel =
                data >= *state_->sel_begin_ and data <= *state_->sel_end_;
        }

        if (*data == newline_glyph_) {

            const utf8::Codepoint c = ' ';

            auto mapping_info = locale_texture_map()(c);
            const u16 t = PLATFORM.map_glyph(c, *mapping_info);

            while (x not_eq calc_screen_tiles().x) {
                PLATFORM.set_tile(Layer::overlay, x, y, t);
                ++x;
            }

            on_newline();

            ++data;
            ++y;

            continue;
        }

        if (skipped < column_offset_) {
            ++skipped;
            handle_char(data, data->cp(*this), ps);
            if (ps.endquote) {
                ps.quotation = false;
            }
            ++data;
            continue;
        }

        const auto c = data->cp(*this);

        handle_char(data, c, ps);

        auto mapping_info = locale_texture_map()(c);

        if (mapping_info) {
            u16 t = PLATFORM.map_glyph(c, *mapping_info);
            if (within_sel) {
                PLATFORM.set_tile(x, y, t, highlight_colors);
            } else if (ps.comment or ps.quotation) {
                PLATFORM.set_tile(
                    x,
                    y,
                    t,
                    FontColors{custom_color(0x99dbff), custom_color(0x007cbf)});
            } else if (ps.keyword and c not_eq '(') {
                PLATFORM.set_tile(
                    x,
                    y,
                    t,
                    FontColors{custom_color(0xfff5b8), custom_color(0x007cbf)});
            } else {
                PLATFORM.set_tile(Layer::overlay, x, y, t);
            }
        }

        if (ps.endquote) {
            ps.quotation = false;
        }


        ++data;
        ++x;
    }

FILL:

    const utf8::Codepoint c = ' ';

    auto mapping_info = locale_texture_map()(c);
    const u16 t = PLATFORM.map_glyph(c, *mapping_info);

    while (y not_eq y_max()) {
        while (x not_eq calc_screen_tiles().x) {
            PLATFORM.set_tile(Layer::overlay, x, y, t);
            ++x;
        }
        ++y;
        x = 0;
    }

    show_status();

    if (show_keyboard_) {
        render_keyboard();
    }

    if (show_completions_) {
        render_completions();
    }
}



StringBuffer<32> TextEditorModule::current_word()
{
    auto begin = text_buffer_.begin();
    auto data = insert_pos();

    --data;

    auto is_delim = [](char c) {
        return c == '\n' or c == ' ' or c == '(' or c == ')';
    };

    while (data not_eq begin and not is_delim(data->cp(*this))) {
        --data;
    }

    if (is_delim(data->cp(*this))) {
        ++data;
    }

    // StringBuffer<32> result;
    // while (*data not_eq '\0' and data not_eq pos) {
    //     result.push_back(*(data++));
    // }

    StringBuffer<32> result;
    while (data->cp(*this) not_eq '\0' and not is_delim(data->cp(*this))) {
        result.push_back((data++)->cp(*this));
    }

    return result;
}



int TextEditorModule::skip_word()
{
    auto data = insert_pos();

    int count = 0;
    while (*data not_eq null_glyph_ and *data not_eq newline_glyph_ and
           data->cp(*this) not_eq ' ') {
        ++count;
        ++data;
    }
    while (data->cp(*this) == ' ') {
        ++count;
        ++data;
    }
    if (count == 0) {
        count = 1;
    }
    return count;
}



int TextEditorModule::back_word()
{
    auto begin = text_buffer_.begin();

    auto data = insert_pos();

    int count = 0;
    while (data not_eq begin and data->cp(*this) not_eq '\0' and
           data->cp(*this) not_eq '\n' and data->cp(*this) not_eq ' ') {
        ++count;
        --data;
    }
    while (data->cp(*this) == ' ') {
        ++count;
        --data;
    }
    if (count == 0) {
        count = 1;
    }
    return count;
}



Vector<TextEditorModule::Glyph>::Iterator TextEditorModule::current_line()
{
    auto data = text_buffer_.begin();

    int line = 0;
    while (*data not_eq null_glyph_ and line not_eq cursor_.y) {
        if (*data == newline_glyph_) {
            ++line;
        }

        ++data;
    }

    return data;
}



int TextEditorModule::line_length()
{
    auto data = current_line();

    int length = 0;

    while (*data not_eq null_glyph_ and *data not_eq newline_glyph_) {
        ++length;
        ++data;
    }

    return length;
}



utf8::Codepoint TextEditorModule::Glyph::cp(TextEditorModule& te) const
{
    return te.glyph_table_[table_entry_];
}



TextEditorModule::Glyph TextEditorModule::load_glyph(utf8::Codepoint c)
{
    if (c < 128) {
        return {(u16)c};
    }
    int i = 0;
    for (auto cp : glyph_table_) {
        if (cp == c) {
            return {(u16)i};
        }
        ++i;
    }
    glyph_table_.push_back(c);
    return {(u16)(glyph_table_.size() - 1)};
}



void TextEditorModule::init_glyph_table()
{
    for (u8 c = 0; c < 128; ++c) {
        glyph_table_.push_back(c);
    }

    newline_glyph_ = load_glyph('\n');
    null_glyph_ = load_glyph('\0');
}



TextEditorModule::TextEditorModule(UserContext&& context)
    : state_(allocate<State>("text-editor-state")),
      user_context_(std::move(context)), filesystem_(FileSystem::device),
      syntax_mode_(SyntaxMode::plain_text), file_mode_(FileMode::readonly)
{
    state_->file_path_ = "/";

    init_glyph_table();

    if (PLATFORM.logger().data()) {
        for (char c : *PLATFORM.logger().data()) {
            text_buffer_.push_back(load_glyph(c));
        }
        text_buffer_.push_back(newline_glyph_);
        text_buffer_.push_back(null_glyph_);
    }
}



TextEditorModule::TextEditorModule(UserContext&& user_context,
                                   const char* file_path,
                                   SyntaxMode syntax_mode,
                                   FileMode file_mode,
                                   FileSystem filesystem)
    : state_(allocate<State>("text-editor-state")),
      user_context_(std::move(user_context)), filesystem_(filesystem),
      syntax_mode_(syntax_mode), file_mode_(file_mode)
{
    state_->file_path_ = file_path;

    init_glyph_table();

    if (file_mode == FileMode::update) {
        if (filesystem_ == FileSystem::sram) {
            Vector<char> tmp_buffer;
            flash_filesystem::read_file_data_text(file_path, tmp_buffer);
            for (char c : tmp_buffer) {
                text_buffer_.push_back(load_glyph(c));
            }
        } else {
            if (file_path[0] == '/') {
                ++file_path;
            }
            auto data = PLATFORM.load_file_contents("", file_path);

            utf8::scan(
                [this](const utf8::Codepoint& cp, const char*, int) {
                    text_buffer_.push_back(load_glyph(cp));
                    return true;
                },
                data,
                strlen(data));
            text_buffer_.push_back(load_glyph('\0'));
        }
    } else {
        text_buffer_.push_back(load_glyph('\n'));
        text_buffer_.push_back(load_glyph('\0'));
    }

    StringBuffer<8> lisp_ext(".lisp");

    if (str_eq(get_extension(file_path), lisp_ext)) {
        // tabs_to_spaces();
    }
}



void TextEditorModule::tabs_to_spaces()
{
    Vector<Glyph> temp_buffer;
    for (Glyph c : text_buffer_) {
        temp_buffer.push_back(c);
    }
    text_buffer_.clear();

    for (Glyph c : temp_buffer) {
        if (c.cp(*this) == '\v') {
            for (int i = 0; i < 3; ++i) {
                text_buffer_.push_back(load_glyph(' '));
            }
        } else if (c.cp(*this) == '\t') {
            for (int i = 0; i < 4; ++i) {
                text_buffer_.push_back(load_glyph(' '));
            }
        } else {
            text_buffer_.push_back(c);
        }
    }
}



void TextEditorModule::enter(Scene& prev)
{
    if (not gui_mode_) {
        PLATFORM.load_overlay_texture("overlay_editor");
    }


    header_.emplace(OverlayCoord{});
    if (not gui_mode_) {
        StringBuffer<32> temp("  text editor  ");
        switch (syntax_mode_) {
        case SyntaxMode::lisp:
            temp += "(lisp mode)";
            break;

        case SyntaxMode::plain_text:
            if (user_context_.readonly_) {
                temp += "(readonly) ";
            } else {
                temp += "(text mode)";
            }
            break;

        case SyntaxMode::python:
            temp += "(python)   ";
            break;

        case SyntaxMode::ini:
            temp += "(ini mode) ";
            break;
        }
        temp += "    ";
        header_->assign(temp.c_str(), status_colors);
    }


    u8 yo = 1;
    if (gui_mode_) {
        yo = 4;
    }
    status_.emplace(OverlayCoord{0, u8((calc_screen_tiles().y - yo))});


    for (Glyph c : text_buffer_) {
        if (c.cp(*this) == '\n') {
            ++line_count_;
        }
    }

    if (not gui_mode_) {
        render(0);
    }
}



void TextEditorModule::repaint()
{
    render(start_line_);
}



void TextEditorModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);
    PLATFORM.screen().clear();
    PLATFORM.screen().display();
    auto clr = ColorConstant::rich_black;
    if (exit_to_browser_) {
        clr = custom_color(0x007cbf);
    }
    PLATFORM.screen().fade(0.9f, clr, {}, true, true);
    PLATFORM.screen().fade(1.f, clr, {}, true, true);

    header_.reset();
    status_.reset();

    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay");
}



ScenePtr TextEditorModule::save()
{
    if (not state_->modified_ and
        not flash_filesystem::file_exists(state_->file_path_.c_str())) {
        return null_scene();
    }

    if (file_mode_ == FileMode::readonly) {
        // Do not save the file
    } else if (filesystem_ == FileSystem::sram) {
        Vector<char> output;
        if (export_as_ascii(output)) {
            flash_filesystem::StorageOptions opts{.use_compression_ = true};
            flash_filesystem::store_file_data_text(
                state_->file_path_.c_str(), output, opts);
        } else {
            PLATFORM.fatal("Text editor cannot save edited file containing "
                           "non-ascii bytes! Sorry!");
        }
    } else {
        Vector<char> output;
        if (export_as_ascii(output)) {
            return make_scene<SramFileWritebackScene>(
                state_->file_path_.c_str(),
                std::move(output),
                std::move(user_context_));
        } else {
            PLATFORM.fatal("Text editor cannot save edited file containing "
                           "non-ascii bytes! Sorry!");
        }
    }

    return null_scene();
}


bool TextEditorModule::export_as_ascii(Vector<char>& output)
{
    for (auto glyph : text_buffer_) {
        if (glyph.cp(*this) > 127) {
            return false;
        }
    }
    for (auto glyph : text_buffer_) {
        output.push_back(glyph.cp(*this));
    }
    return true;
}


void TextEditorModule::copy_selected(Vector<utf8::Codepoint>& output)
{
    output.clear();
    if (state_->sel_begin_) {
        save_selection(output);
        deselect();
    }
}



void TextEditorModule::paste(Vector<utf8::Codepoint>& contents)
{
    paste_selection(contents);
    render(start_line_);
}



StringBuffer<64> TextEditorModule::file_path()
{
    return state_->file_path_;
}



void TextEditorModule::deselect()
{
    state_->sel_begin_.reset();
    state_->sel_end_.reset();
    state_->sel_center_.reset();
    render(start_line_);
    shade_cursor();
}



void TextEditorModule::shade_cursor()
{
    cursor_shaded_ = true;

    const auto x = cursor_.x - column_offset_;
    const auto y = (cursor_.y - start_line_) + cursor_y_offset();

    stashed_palette_ = PLATFORM.get_palette(Layer::overlay, x, y);

    const auto t = PLATFORM.get_tile(Layer::overlay, x, y);
    PLATFORM.set_tile(x, y, t, highlight_colors);
}



bool TextEditorModule::should_smooth_scroll() const
{
    return not PLATFORM.has_slow_cpu() or text_buffer_.size() < 4000;
}



// NOTE: while the text editor code in general isn't too bad, this function in
// particular is a cluttered mess of copy-pasted code, mainly due to related but
// slightly differing behavior in all of the different keyboard shortcuts and
// editing modes.
ScenePtr TextEditorModule::update(Time delta)
{
    APP.player().update(delta);

    auto unshade_cursor = [&] {
        cursor_shaded_ = false;

        const auto x = cursor_.x - column_offset_;
        const auto y = (cursor_.y - start_line_) + cursor_y_offset();

        const auto t = PLATFORM.get_tile(Layer::overlay, x, y);
        PLATFORM.set_tile(Layer::overlay, x, y, t);

        PLATFORM.set_palette(Layer::overlay, x, y, stashed_palette_);
    };


    auto selected = [&]() -> bool {
        return static_cast<bool>(state_->sel_begin_);
    };

    auto sel_forward = [&](bool& do_render) {
        if (selected()) {
            auto pos = insert_pos();
            if (state_->sel_center_ < pos) {
                state_->sel_end_ = pos;
                state_->sel_begin_ = state_->sel_center_;
            } else {
                state_->sel_end_ = state_->sel_center_;
                state_->sel_begin_ = pos;
            }

            do_render = true;
        }
    };

    auto sel_backward = [&](bool& do_render) {
        if (selected()) {
            auto pos = insert_pos();
            if (state_->sel_center_ < pos) {
                state_->sel_end_ = pos;
                state_->sel_begin_ = state_->sel_center_;
            } else {
                state_->sel_end_ = state_->sel_center_;
                state_->sel_begin_ = pos;
            }
            do_render = true;
        }
    };

    auto center_view = [&] {
        u8 yo = 2;
        if (gui_mode_) {
            yo = 5;
        }

        start_line_ = std::max(0, cursor_.y - ((y_max() - yo) / 2));
        if (cursor_.x > 15) {
            column_offset_ = cursor_.x - 15;
        } else {
            column_offset_ = 0;
        }
        render(start_line_);
        shade_cursor();
    };

    if (APP.player().button_down(Button::select)) {
        if (selected()) {
            deselect();
        } else {
            state_->sel_begin_ = insert_pos();
            state_->sel_end_ = state_->sel_begin_;
            state_->sel_center_ = state_->sel_begin_;
        }
    }

    switch (mode_) {
    case Mode::nav:
        cursor_flicker_timer_ += delta;
        if (cursor_flicker_timer_ > milliseconds(200)) {
            cursor_flicker_timer_ = 0;

            if (cursor_shaded_) {
                unshade_cursor();
            } else {
                shade_cursor();
            }
        }

        if (APP.player().button_down(Button::alt_2)) {
            if (APP.player().button_pressed(Button::alt_1)) {
                center_view();
            }
        } else if (APP.player().button_pressed(Button::alt_2)) {
            if (APP.player().button_down(Button::alt_1)) {
                center_view();
            } else if (APP.player().button_down(Button::right)) {
                unshade_cursor();
                bool do_render = false;
                cursor_.x += skip_word();
                cursor_.x = std::min(cursor_.x, line_length());
                ideal_cursor_right_ = cursor_.x;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                    do_render = true;
                }
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                    do_render = true;
                }
                sel_forward(do_render);
                if (do_render) {
                    render(start_line_);
                }
                shade_cursor();
            } else if (APP.player().button_down(Button::left)) {
                unshade_cursor();
                bool do_render = false;
                cursor_.x -= back_word();
                cursor_.x = std::max(cursor_.x, 0);
                ideal_cursor_right_ = cursor_.x;
                if (cursor_.x > column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                    do_render = true;
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                    do_render = true;
                }
                sel_backward(do_render);
                if (do_render) {
                    render(start_line_);
                }
                shade_cursor();
            } else if (APP.player().button_down(Button::down)) {

                if (cursor_shaded_) {
                    unshade_cursor();
                }

                cursor_.x = 0;

                while (*insert_pos() == newline_glyph_) {
                    ++cursor_.y;
                }

                while (cursor_.y not_eq line_count_) {
                    ++cursor_.y;
                    if (*insert_pos() == newline_glyph_) {
                        break;
                    }
                }

                bool do_render = false;

                u8 yo = 2;
                if (gui_mode_) {
                    yo = 5;
                }

                if (cursor_.y > start_line_ + (y_max() - yo)) {
                    start_line_ = std::max(0, cursor_.y - ((y_max() - yo) / 2));
                    do_render = true;
                }

                if (cursor_.x < column_offset_) {
                    column_offset_ = 0;
                    do_render = true;
                }

                sel_forward(do_render);

                if (do_render) {
                    render(start_line_);
                } else {
                    show_status();
                }

                shade_cursor();

            } else if (APP.player().button_down(Button::up)) {

                if (cursor_.y == 0) {
                    return null_scene();
                }

                if (cursor_shaded_) {
                    unshade_cursor();
                }

                cursor_.x = 0;

                while (insert_pos()->cp(*this) == '\n') {
                    --cursor_.y;
                }

                while (cursor_.y not_eq 0) {
                    --cursor_.y;
                    if (insert_pos()->cp(*this) == '\n') {
                        break;
                    }
                }

                bool do_render = false;

                if (cursor_.y < start_line_) {
                    start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
                    do_render = true;
                }

                if (cursor_.x < column_offset_) {
                    column_offset_ = 0;
                    do_render = true;
                }

                sel_backward(do_render);

                if (do_render) {
                    render(start_line_);
                } else {
                    show_status();
                }

                shade_cursor();
            } else if (APP.player().button_down(Button::action_1)) {
                user_context_.yank_buffer_.reset();
                if (state_->sel_begin_) {
                    user_context_.yank_buffer_.emplace();
                    copy_selected(*user_context_.yank_buffer_);
                }
            } else if (APP.player().button_down(Button::action_2)) {
                if (user_context_.yank_buffer_) {
                    paste(*user_context_.yank_buffer_);
                }
            }
        } else if (APP.player().button_pressed(Button::alt_1)) {
            if (APP.player().button_down(Button::action_1) and
                not user_context_.readonly_) {
                auto pos = insert_pos();
                insert_char(load_glyph('\n'), pos);
                cursor_.x = 0; // newline
                cursor_.y += 1;
                ++pos;
                int paren_balance = 0;
                auto begin = text_buffer_.begin();
                while (begin not_eq pos) {
                    if (begin->cp(*this) == '(') {
                        ++paren_balance;
                    }
                    if (begin->cp(*this) == ')') {
                        --paren_balance;
                    }
                    ++begin;
                }
                while (paren_balance) {
                    insert_char(load_glyph(' '), pos++);
                    ++cursor_.x;
                    --paren_balance;
                }
                render(start_line_);
                shade_cursor();
            } else if (APP.player().button_down(Button::action_2) and
                       not user_context_.readonly_) {
                if (selected()) {
                    delete_selection();
                } else {
                    erase_char();
                    cursor_.x -= 1;
                }

                if (cursor_.x == -1) {
                    cursor_.y -= 1;
                    cursor_.x = line_length();
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                }
                render(start_line_);
                shade_cursor();
            } else if (APP.player().button_down(Button::right)) {
                cursor_.x = line_length();
                ideal_cursor_right_ = cursor_.x;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                }
                bool dummy;
                sel_forward(dummy);
                render(start_line_);
                shade_cursor();
            } else if (APP.player().button_down(Button::left)) {
                cursor_.x = 0;
                ideal_cursor_right_ = cursor_.x;
                if (cursor_.x > column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                bool dummy;
                sel_backward(dummy);
                render(start_line_);
                shade_cursor();
            } else if (APP.player().button_down(Button::down)) {
                cursor_.x = 0;
                cursor_.y = line_count_;
                column_offset_ = 0;
                u8 yo = 3;
                if (gui_mode_) {
                    yo = 5;
                }
                start_line_ =
                    std::max(0, line_count_ - (calc_screen_tiles().y - yo));
                bool dummy;
                sel_forward(dummy);
                render(start_line_);
                shade_cursor();
            } else if (APP.player().button_down(Button::up)) {
                cursor_.x = 0;
                cursor_.y = 0;
                column_offset_ = 0;
                start_line_ = 0;
                bool dummy;
                sel_backward(dummy);
                render(start_line_);
                shade_cursor();
            }
        } else if ((APP.player().button_down(Button::up) or
                    (APP.player().button_held(Button::up,
                                              milliseconds(400)))) and
                   cursor_.y > 0) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            --cursor_.y;

            APP.player().button_held_reset(Button::up, milliseconds(60));

            bool do_render = false;

            if (cursor_.y < start_line_) {
                if (should_smooth_scroll()) {
                    --start_line_;
                } else {
                    start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
                }
                do_render = true;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                    do_render = true;
                }
            }

            while (cursor_.x > column_offset_ + (calc_screen_tiles().x - 1)) {
                do_render = true;
                ++column_offset_;
            }

            sel_backward(do_render);

            if (do_render) {
                render(start_line_);
            }

            shade_cursor();
            show_status();
        } else if ((APP.player().button_down(Button::down) or
                    (APP.player().button_held(Button::down,
                                              milliseconds(400)))) and
                   cursor_.y < line_count_) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            ++cursor_.y;

            bool do_render = false;

            APP.player().button_held_reset(Button::down, milliseconds(60));

            u8 yo = 2;
            if (gui_mode_) {
                yo = 5;
            }

            if (cursor_.y > start_line_ + (y_max() - yo)) {
                if (should_smooth_scroll()) {
                    ++start_line_;
                } else {
                    start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
                }
                do_render = true;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                    do_render = true;
                }
            }

            while (cursor_.x > column_offset_ + (calc_screen_tiles().x - 1)) {
                ++column_offset_;
                do_render = true;
            }

            sel_forward(do_render);

            if (do_render) {
                render(start_line_);
            }

            shade_cursor();
            show_status();
        } else if (APP.player().button_down(Button::right) or
                   (APP.player().button_held(Button::right,
                                             milliseconds(400)))) {

            APP.player().button_held_reset(Button::right, milliseconds(60));

            if (line_length() > cursor_.x) {
                unshade_cursor();
                cursor_flicker_timer_ = -seconds(1);

                ++cursor_.x;

                bool do_render = false;

                ideal_cursor_right_ = cursor_.x;
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                    do_render = true;
                }

                sel_forward(do_render);

                if (do_render) {
                    render(start_line_);
                }

                shade_cursor();
                show_status();
            } else if (cursor_.y < line_count_) {
                // We're at the end of the line, jump to the beginning of the
                // next one.
                unshade_cursor();
                cursor_flicker_timer_ = -seconds(1);
                cursor_.x = 0;
                const auto old_column_offset = column_offset_;
                column_offset_ = 0;

                bool do_render = false;

                if (cursor_.x < old_column_offset or
                    old_column_offset not_eq 0) {
                    do_render = true;
                }

                ++cursor_.y;

                if (cursor_.y > start_line_ + (calc_screen_tiles().y - 3)) {
                    ++start_line_;
                    do_render = true;
                }

                sel_forward(do_render);

                if (do_render) {
                    render(start_line_);
                }

                shade_cursor();
                show_status();
            }
        } else if (APP.player().button_down(Button::left) or
                   (APP.player().button_held(Button::left,
                                             milliseconds(400)))) {

            if (cursor_.x > 0) {
                bool do_render = false;

                APP.player().button_held_reset(Button::left, milliseconds(60));

                unshade_cursor();
                cursor_flicker_timer_ = -seconds(1);

                --cursor_.x;

                ideal_cursor_right_ = cursor_.x;
                while (cursor_.x < column_offset_) {
                    --column_offset_;
                    do_render = true;
                }

                sel_backward(do_render);

                if (do_render) {
                    render(start_line_);
                }

                shade_cursor();
                show_status();
            } else if (cursor_.y > 0) {
                unshade_cursor();
                cursor_flicker_timer_ = -seconds(1);

                --cursor_.y;
                cursor_.x = line_length();

                ideal_cursor_right_ = cursor_.x;

                bool do_render = false;

                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                    do_render = true;
                }
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                    do_render = true;
                }

                if (cursor_.y < start_line_) {
                    --start_line_;
                    do_render = true;
                }

                sel_backward(do_render);

                if (do_render) {
                    render(start_line_);
                }

                shade_cursor();
                show_status();
            }

        } else if (APP.player().button_down(Button::action_2)) {
            if (selected()) {
                deselect();
            } else {
                if (state_->modified_) {
                    if (auto ret = save()) {
                        return ret;
                    }
                }
                exit_to_browser_ = true;
                if (filesystem_ == FileSystem::device) {
                    return make_scene<FileBrowserModule>();
                }
                auto next = make_scene<FileBrowserModule>(
                    std::move(user_context_),
                    state_->file_path_.c_str(),
                    filesystem_ == FileSystem::rom);
                if (browser_index_ > -1) {
                    next->scroll_index_ = browser_index_;
                }
                return next;
            }
        } else if (APP.player().button_down(Button::action_1) and
                   not user_context_.readonly_ and
                   file_mode_ not_eq FileMode::readonly) {
            start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
            show_keyboard_ = true;
            mode_ = Mode::edit;
            keyboard_cursor_ = {5, 4};
            render(start_line_);
            shade_cursor();
        }
        break;

    case Mode::edit:
        if (APP.player().button_down(Button::action_2)) {
            mode_ = Mode::nav;
            show_keyboard_ = false;
            render(start_line_);
            shade_cursor();
        } else if (APP.player().button_down(Button::left)) {
            if (keyboard_cursor_.x == 0) {
                keyboard_cursor_.x = 6;
            } else {
                --keyboard_cursor_.x;
            }
            render_keyboard();
        } else if (APP.player().button_down(Button::right)) {
            if (keyboard_cursor_.x == 6) {
                keyboard_cursor_.x = 0;
            } else {
                ++keyboard_cursor_.x;
            }
            render_keyboard();
        } else if (APP.player().button_down(Button::up)) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                --keyboard_cursor_.y;
            }
            render_keyboard();
        } else if (APP.player().button_down(Button::down)) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                ++keyboard_cursor_.y;
            }
            render_keyboard();
        } else if (APP.player().button_down(Button::action_1)) {
            if (keyboard_cursor_.y == 5 and keyboard_cursor_.x == 6) {
                erase_char();
                cursor_.x -= 1;
                if (cursor_.x == -1) {
                    cursor_.y -= 1;
                    cursor_.x = line_length();
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                while (cursor_.x >
                       column_offset_ + (calc_screen_tiles().x - 1)) {
                    ++column_offset_;
                }
            } else {
                auto do_insert = [&](Glyph c) {
                    insert_char(c);
                    if (c.cp(*this) == '\n') {
                        cursor_.x = 0;
                        cursor_.y += 1;
                    } else {
                        cursor_.x += 1;
                    }

                    if (cursor_.x >
                        column_offset_ + (calc_screen_tiles().x - 1)) {
                        ++column_offset_;
                    }
                    if (cursor_.x < column_offset_) {
                        column_offset_ = cursor_.x;
                    }
                    ideal_cursor_right_ = cursor_.x;
                };
                char c = keyboard[keyboard_cursor_.y][keyboard_cursor_.x][0];
                do_insert(load_glyph(c));

                if (c == '(') {
                    do_insert(load_glyph(')'));
                    --cursor_.x;
                } else if (c == '"') {
                    do_insert(load_glyph('"'));
                    --cursor_.x;
                }
            }
            render(start_line_);
            shade_cursor();
        } else if (APP.player().button_down(Button::alt_1)) {

            state_->current_word_ = current_word();
            state_->completions_.clear();

            // state_->completions_.push_back(state_->current_word_.c_str());

            auto handle_completion_word = [&](const char* word) {
                if (state_->completions_.full()) {
                    return;
                }

                const auto intern_len = strlen(word);
                if (intern_len <= state_->current_word_.length()) {
                    return;
                }

                for (u32 i = 0; i < state_->current_word_.length(); ++i) {
                    if (state_->current_word_[i] not_eq word[i]) {
                        return;
                    }
                }

                for (auto& existing : state_->completions_) {
                    if (existing == word) {
                        return;
                    }
                }

                state_->completions_.push_back(word);
            };


            parse_words(*this, text_buffer_.begin(), [&](auto& word) {
                if (state_->current_word_.empty() and is_numeric(word)) {
                    // Just out of personal preference, do not add integers from
                    // the current text buffer to the list of completions if the
                    // user has not yet entered any text to complete. Kind of an
                    // obscure edge case, but I just think it looks weird if you
                    // hit autocomplete, even with your cursor surrounded on
                    // both sides by whitespace, and see integers in the
                    // autocomplete window.
                    return;
                }

                handle_completion_word(word.c_str());
            });


            lisp::get_env(handle_completion_word);
            lisp::get_interns(handle_completion_word);

            mode_ = Mode::autocomplete;
            show_keyboard_ = false;
            show_completions_ = true;
            selected_completion_ = 0;
            render(start_line_);
            shade_cursor();
        }
        break;

    case Mode::autocomplete:
        if (APP.player().button_down(Button::action_2)) {
            mode_ = Mode::edit;
            show_keyboard_ = true;
            show_completions_ = false;
            render(start_line_);
            shade_cursor();
        } else if (APP.player().button_down(Button::up)) {
            if (selected_completion_ > 0) {
                --selected_completion_;
                render_completions();
            }
        } else if (APP.player().button_down(Button::down)) {
            if (selected_completion_ < (int)state_->completions_.size() - 1) {
                ++selected_completion_;
                render_completions();
            }
        } else if (APP.player().button_down(Button::action_1)) {
            mode_ = Mode::edit;

            auto cpl = state_->completions_[selected_completion_];
            auto insert = insert_pos();
            for (u32 i = state_->current_word_.length(); i < cpl.length();
                 ++i) {
                insert_char(load_glyph(cpl[i]), insert);
                ++cursor_.x;
                ++insert;
            }

            show_completions_ = false;
            show_keyboard_ = true;
            render(start_line_);
            shade_cursor();
        }

        break;
    }

    return null_scene();
}



TextEditorModule::FileSystem TextEditorModule::which_fs() const
{
    return filesystem_;
}



StringBuffer<32> TextEditorModule::extract_filename(const char* path)
{
    const char* p = path;
    int dir_count = 0;
    while (*p not_eq '\0') {
        if (*p == '/') {
            ++dir_count;
        }
        ++p;
    }
    if (dir_count > 0) {
        while (*p not_eq '/') {
            --p;
        }
        ++p;
    }
    StringBuffer<32> result;
    if (*p == '\0') {
        result = "SYSLOG";
    } else {
        result = p;
    }
    return result;
}



Vector<TextEditorModule::Glyph>::Iterator TextEditorModule::insert_pos()
{
    auto data = text_buffer_.begin();

    int line = cursor_.y;
    int offset = cursor_.x;

    while (line) {
        if (*data == null_glyph_) {
            break;
        }

        if (*data == newline_glyph_) {
            --line;
        }

        ++data;
    }

    while (offset) {
        if (*data == null_glyph_ or *data == newline_glyph_) {
            break;
        }

        --offset;
        ++data;
    }

    return data;
}



void TextEditorModule::erase_char(Optional<Vector<Glyph>::Iterator> hint)
{
    if (cursor_.x == 0 and cursor_.y == 0) {
        // Nothing left to delete.
        return;
    }

    state_->modified_ = true;

    auto begin = hint ? *hint : --(insert_pos());

    if (begin->cp(*this) == '\n') {
        --line_count_;
    }

    text_buffer_.erase(begin);
}



void TextEditorModule::delete_selection()
{
    int cursor_y_shift = 0;

    while (*state_->sel_end_ not_eq *state_->sel_begin_) {
        if ((*state_->sel_end_)->cp(*this) == '\n') {
            ++cursor_y_shift;
        }
        // TODO: erase range instead, this repeated erase call is very slow.
        erase_char(*state_->sel_end_);
        --(*state_->sel_end_);
    }
    if ((*state_->sel_end_)->cp(*this) == '\n') {
        ++cursor_y_shift;
    }

    int cursor_x = -1;
    auto temp = *state_->sel_begin_;
    while (temp not_eq text_buffer_.begin() and temp->cp(*this) not_eq '\n') {
        // Seek backwards to the beginning of the current line.
        --temp;
        ++cursor_x;
    }

    erase_char(*state_->sel_begin_);
    state_->sel_begin_.reset();
    state_->sel_end_.reset();

    cursor_.x = cursor_x;
    cursor_.y -= cursor_y_shift;

    if (cursor_.y > start_line_ + (calc_screen_tiles().y - 3) or
        cursor_.y < start_line_) {
        start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
    }

    if (cursor_.x < column_offset_) {
        column_offset_ = cursor_.x;
    }
    while (cursor_.x > column_offset_ + (calc_screen_tiles().x - 1)) {
        ++column_offset_;
    }
}



void TextEditorModule::paste_selection(Vector<utf8::Codepoint>& source)
{
    if (state_->sel_begin_) {
        delete_selection();
    }

    auto insert = insert_pos();

    for (auto cp : source) {
        PLATFORM_EXTENSION(feed_watchdog);
        auto glyph = load_glyph(cp);
        insert_char(glyph, insert);
        if (cp == '\n') {
            ++cursor_.y;
            cursor_.x = 0;
            // Because we jumped down a line, we want to re-adjust the insert
            // pos?
            insert = insert_pos();
        } else {
            ++cursor_.x;
            ++insert;
        }
    }
}



void TextEditorModule::save_selection(Vector<utf8::Codepoint>& output)
{
    if (not state_->sel_begin_ or not state_->sel_end_) {
        return;
    }

    auto begin = *state_->sel_begin_;
    auto end = *state_->sel_end_;

    if (begin >= end) {
        // Raise error? Should never happen...
        return;
    }

    while (begin not_eq end) {
        output.push_back(begin->cp(*this));
        ++begin;
    }

    output.push_back(end->cp(*this));
}



void TextEditorModule::insert_char(Glyph c,
                                   Optional<Vector<Glyph>::Iterator> hint)
{
    state_->modified_ = true;

    if (c.cp(*this) == '\n') {
        ++line_count_;
    }

    if (state_->sel_begin_) {
        delete_selection();
    }

    auto begin = hint ? *hint : insert_pos();

    text_buffer_.insert(begin, c);
}



// TextEditorModule::Factory TextEditorModule::factory_;



} // namespace skyland
