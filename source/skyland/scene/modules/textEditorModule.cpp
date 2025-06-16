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



void TextEditorModule::handle_char(Vector<char>::Iterator data,
                                   char c,
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
            if (*iter_cpy == '\n') {
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

            while (*seek not_eq '\0' and *seek not_eq ' ' and
                   *seek not_eq '(' and *seek not_eq ')' and
                   *seek not_eq '\n') {
                word.push_back(*seek);
                ++seek;
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

            while (*seek not_eq '\0' and *seek not_eq ' ' and
                   *seek not_eq '(' and *seek not_eq ')' and
                   *seek not_eq '\n') {
                word.push_back(*seek);
                ++seek;
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
void parse_words(TextEditorModule& m, Vector<char>::Iterator data, F&& callback)
{
    TextEditorModule::ParserState ps;

    while (*data not_eq '\0') {

        ps.endquote = false;

        if (*data == '\n') {
            ps = TextEditorModule::ParserState{};
        } else {
            m.handle_char(data, *data, ps);
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
        if (*data == '\0') {
            break;
        }

        if (*data == '\n') {
            --start_line;
        }

        ++data;
    }

    int skipped = 0;

    ParserState ps;


    while (*data not_eq '\0' and y not_eq y_max()) {

        ps.endquote = false;

        auto on_newline = [&] {
            x = 0;
            skipped = 0;
            ps.comment = false;
            ps.quotation = false;
        };

        if (x == calc_screen_tiles().x) {
            while (*data not_eq '\n') {
                if (*data == '\0') {
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

        if (*data == '\n') {

            const char c = ' ';

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
            handle_char(data, *data, ps);
            if (ps.endquote) {
                ps.quotation = false;
            }
            ++data;
            continue;
        }

        const char c = *data;

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

    const char c = ' ';

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

    while (data not_eq begin and not is_delim(*data)) {
        --data;
    }

    if (is_delim(*data)) {
        ++data;
    }

    // StringBuffer<32> result;
    // while (*data not_eq '\0' and data not_eq pos) {
    //     result.push_back(*(data++));
    // }

    StringBuffer<32> result;
    while (*data not_eq '\0' and not is_delim(*data)) {
        result.push_back(*(data++));
    }

    return result;
}



int TextEditorModule::skip_word()
{
    auto data = insert_pos();

    int count = 0;
    while (*data not_eq '\0' and *data not_eq '\n' and *data not_eq ' ') {
        ++count;
        ++data;
    }
    while (*data == ' ') {
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
    while (data not_eq begin and *data not_eq '\0' and *data not_eq '\n' and
           *data not_eq ' ') {
        ++count;
        --data;
    }
    while (*data == ' ') {
        ++count;
        --data;
    }
    if (count == 0) {
        count = 1;
    }
    return count;
}



Vector<char>::Iterator TextEditorModule::current_line()
{
    auto data = text_buffer_.begin();

    int line = 0;
    while (*data not_eq '\0' and line not_eq cursor_.y) {
        if (*data == '\n') {
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

    while (*data not_eq '\0' and *data not_eq '\n') {
        ++length;
        ++data;
    }

    return length;
}



TextEditorModule::TextEditorModule(UserContext&& context)
    : state_(allocate_dynamic<State>("text-editor-state")),
      user_context_(std::move(context)), filesystem_(FileSystem::device),
      syntax_mode_(SyntaxMode::plain_text), file_mode_(FileMode::readonly)
{
    state_->file_path_ = "/";

    if (PLATFORM.logger().data()) {
        for (char c : *PLATFORM.logger().data()) {
            text_buffer_.push_back(c);
        }
        text_buffer_.push_back('\n');
        text_buffer_.push_back('\0');
    }
}



TextEditorModule::TextEditorModule(UserContext&& user_context,
                                   const char* file_path,
                                   SyntaxMode syntax_mode,
                                   FileMode file_mode,
                                   FileSystem filesystem)
    : state_(allocate_dynamic<State>("text-editor-state")),
      user_context_(std::move(user_context)), filesystem_(filesystem),
      syntax_mode_(syntax_mode), file_mode_(file_mode)
{
    state_->file_path_ = file_path;

    if (file_mode == FileMode::update) {
        if (filesystem_ == FileSystem::sram) {
            flash_filesystem::read_file_data_text(file_path, text_buffer_);
        } else {
            if (file_path[0] == '/') {
                ++file_path;
            }
            auto data = PLATFORM.load_file_contents("", file_path);

            while (*data not_eq '\0') {
                text_buffer_.push_back(*(data++));
            }
            text_buffer_.push_back('\0');
        }
    } else {
        text_buffer_.push_back('\n');
        text_buffer_.push_back('\0');
    }

    StringBuffer<8> lisp_ext(".lisp");

    if (str_eq(get_extension(file_path), lisp_ext)) {
        // tabs_to_spaces();
    }
}



bool TextEditorModule::has_text()
{
    return not text_buffer_.size() not_eq 0 and text_buffer_[0] not_eq '\0';
}



void TextEditorModule::tabs_to_spaces()
{
    Vector<char> temp_buffer;
    for (char c : text_buffer_) {
        temp_buffer.push_back(c);
    }
    text_buffer_.clear();

    for (char c : temp_buffer) {
        if (c == '\v') {
            for (int i = 0; i < 3; ++i) {
                text_buffer_.push_back(' ');
            }
        } else if (c == '\t') {
            for (int i = 0; i < 4; ++i) {
                text_buffer_.push_back(' ');
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


    for (char c : text_buffer_) {
        if (c == '\n') {
            ++line_count_;
        }
    }

    // render(0);
}
// /scripts/event/quest/human/0.lisp


void TextEditorModule::repaint()
{
    render(start_line_);
}



void TextEditorModule::exit(Scene& next)
{
    PLATFORM_EXTENSION(force_vsync);
    PLATFORM.screen().fade(0.9f, ColorConstant::rich_black, {}, true, true);
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

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
        flash_filesystem::StorageOptions opts{.use_compression_ = true};
        flash_filesystem::store_file_data_text(
            state_->file_path_.c_str(), text_buffer_, opts);
    } else {
        return make_scene<SramFileWritebackScene>(state_->file_path_.c_str(),
                                                  std::move(text_buffer_),
                                                  std::move(user_context_));
    }

    return null_scene();
}



void TextEditorModule::copy_selected(Vector<char>& output)
{
    output.clear();
    if (state_->sel_begin_) {
        save_selection(output);
        deselect();
    }
}



void TextEditorModule::paste(Vector<char>& contents)
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

    if (APP.player().key_down(Key::select)) {
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

        if (APP.player().key_down(Key::alt_2)) {
            if (APP.player().key_pressed(Key::alt_1)) {
                center_view();
            }
        } else if (APP.player().key_pressed(Key::alt_2)) {
            if (APP.player().key_down(Key::alt_1)) {
                center_view();
            } else if (APP.player().key_down(Key::right)) {
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
            } else if (APP.player().key_down(Key::left)) {
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
            } else if (APP.player().key_down(Key::down)) {

                if (cursor_shaded_) {
                    unshade_cursor();
                }

                cursor_.x = 0;

                while (*insert_pos() == '\n') {
                    ++cursor_.y;
                }

                while (cursor_.y not_eq line_count_) {
                    ++cursor_.y;
                    if (*insert_pos() == '\n') {
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

            } else if (APP.player().key_down(Key::up)) {

                if (cursor_.y == 0) {
                    return null_scene();
                }

                if (cursor_shaded_) {
                    unshade_cursor();
                }

                cursor_.x = 0;

                while (*insert_pos() == '\n') {
                    --cursor_.y;
                }

                while (cursor_.y not_eq 0) {
                    --cursor_.y;
                    if (*insert_pos() == '\n') {
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
            } else if (APP.player().key_down(Key::action_1)) {
                user_context_.yank_buffer_.reset();
                if (state_->sel_begin_) {
                    user_context_.yank_buffer_.emplace();
                    copy_selected(*user_context_.yank_buffer_);
                }
            } else if (APP.player().key_down(Key::action_2)) {
                if (user_context_.yank_buffer_) {
                    paste(*user_context_.yank_buffer_);
                }
            }
        } else if (APP.player().key_pressed(Key::alt_1)) {
            if (APP.player().key_down(Key::action_1) and
                not user_context_.readonly_) {
                auto pos = insert_pos();
                insert_char('\n', pos);
                cursor_.x = 0; // newline
                cursor_.y += 1;
                ++pos;
                int paren_balance = 0;
                auto begin = text_buffer_.begin();
                while (begin not_eq pos) {
                    if (*begin == '(') {
                        ++paren_balance;
                    }
                    if (*begin == ')') {
                        --paren_balance;
                    }
                    ++begin;
                }
                while (paren_balance) {
                    insert_char(' ', pos++);
                    ++cursor_.x;
                    --paren_balance;
                }
                render(start_line_);
                shade_cursor();
            } else if (APP.player().key_down(Key::action_2) and
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
            } else if (APP.player().key_down(Key::right)) {
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
            } else if (APP.player().key_down(Key::left)) {
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
            } else if (APP.player().key_down(Key::down)) {
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
            } else if (APP.player().key_down(Key::up)) {
                cursor_.x = 0;
                cursor_.y = 0;
                column_offset_ = 0;
                start_line_ = 0;
                bool dummy;
                sel_backward(dummy);
                render(start_line_);
                shade_cursor();
            }
        } else if ((APP.player().key_down(Key::up) or
                    (APP.player().key_held(Key::up, milliseconds(400)))) and
                   cursor_.y > 0) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            --cursor_.y;

            APP.player().key_held_reset(Key::up, milliseconds(60));

            bool do_render = false;

            if (cursor_.y < start_line_) {
                start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
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
        } else if ((APP.player().key_down(Key::down) or
                    (APP.player().key_held(Key::down, milliseconds(400)))) and
                   cursor_.y < line_count_) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            ++cursor_.y;

            bool do_render = false;

            APP.player().key_held_reset(Key::down, milliseconds(60));

            u8 yo = 2;
            if (gui_mode_) {
                yo = 5;
            }

            if (cursor_.y > start_line_ + (y_max() - yo)) {
                start_line_ = std::max(0, cursor_.y - ((y_max() - 2) / 2));
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
        } else if (APP.player().key_down(Key::right) or
                   (APP.player().key_held(Key::right, milliseconds(400)))) {

            APP.player().key_held_reset(Key::right, milliseconds(60));

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
        } else if (APP.player().key_down(Key::left) or
                   (APP.player().key_held(Key::left, milliseconds(400)))) {

            if (cursor_.x > 0) {
                bool do_render = false;

                APP.player().key_held_reset(Key::left, milliseconds(60));

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

        } else if (APP.player().key_down(Key::action_2)) {
            if (selected()) {
                deselect();
            } else {
                if (state_->modified_) {
                    if (auto ret = save()) {
                        return ret;
                    }
                }
                if (filesystem_ == FileSystem::device) {
                    return make_scene<FileBrowserModule>();
                }
                return make_scene<FileBrowserModule>(std::move(user_context_),
                                                     state_->file_path_.c_str(),
                                                     filesystem_ ==
                                                         FileSystem::rom);
            }
        } else if (APP.player().key_down(Key::action_1) and
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
        if (APP.player().key_down(Key::action_2)) {
            mode_ = Mode::nav;
            show_keyboard_ = false;
            render(start_line_);
            shade_cursor();
        } else if (APP.player().key_down(Key::left)) {
            if (keyboard_cursor_.x == 0) {
                keyboard_cursor_.x = 6;
            } else {
                --keyboard_cursor_.x;
            }
            render_keyboard();
        } else if (APP.player().key_down(Key::right)) {
            if (keyboard_cursor_.x == 6) {
                keyboard_cursor_.x = 0;
            } else {
                ++keyboard_cursor_.x;
            }
            render_keyboard();
        } else if (APP.player().key_down(Key::up)) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                --keyboard_cursor_.y;
            }
            render_keyboard();
        } else if (APP.player().key_down(Key::down)) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                ++keyboard_cursor_.y;
            }
            render_keyboard();
        } else if (APP.player().key_down(Key::action_1)) {
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
                auto do_insert = [&](char c) {
                    insert_char(c);
                    if (c == '\n') {
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
                do_insert(c);

                if (c == '(') {
                    do_insert(')');
                    --cursor_.x;
                } else if (c == '"') {
                    do_insert('"');
                    --cursor_.x;
                }
            }
            render(start_line_);
            shade_cursor();
        } else if (APP.player().key_down(Key::alt_1)) {

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
        if (APP.player().key_down(Key::action_2)) {
            mode_ = Mode::edit;
            show_keyboard_ = true;
            show_completions_ = false;
            render(start_line_);
            shade_cursor();
        } else if (APP.player().key_down(Key::up)) {
            if (selected_completion_ > 0) {
                --selected_completion_;
                render_completions();
            }
        } else if (APP.player().key_down(Key::down)) {
            if (selected_completion_ < (int)state_->completions_.size() - 1) {
                ++selected_completion_;
                render_completions();
            }
        } else if (APP.player().key_down(Key::action_1)) {
            mode_ = Mode::edit;

            auto cpl = state_->completions_[selected_completion_];
            auto insert = insert_pos();
            for (u32 i = state_->current_word_.length(); i < cpl.length();
                 ++i) {
                insert_char(cpl[i], insert);
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



Vector<char>::Iterator TextEditorModule::insert_pos()
{
    auto data = text_buffer_.begin();

    int line = cursor_.y;
    int offset = cursor_.x;

    while (line) {
        if (*data == '\0') {
            break;
        }

        if (*data == '\n') {
            --line;
        }

        ++data;
    }

    while (offset) {
        if (*data == '\0' or *data == '\n') {
            break;
        }

        --offset;
        ++data;
    }

    return data;
}



void TextEditorModule::erase_char(Optional<Vector<char>::Iterator> hint)
{
    if (cursor_.x == 0 and cursor_.y == 0) {
        // Nothing left to delete.
        return;
    }

    state_->modified_ = true;

    auto begin = hint ? *hint : --(insert_pos());

    if (*begin == '\n') {
        --line_count_;
    }

    text_buffer_.erase(begin);
}



void TextEditorModule::delete_selection()
{
    int cursor_y_shift = 0;

    while (*state_->sel_end_ not_eq *state_->sel_begin_) {
        if (**state_->sel_end_ == '\n') {
            ++cursor_y_shift;
        }
        // TODO: erase range instead, this repeated erase call is very slow.
        erase_char(*state_->sel_end_);
        --(*state_->sel_end_);
    }
    if (**state_->sel_end_ == '\n') {
        ++cursor_y_shift;
    }

    int cursor_x = -1;
    auto temp = *state_->sel_begin_;
    while (temp not_eq text_buffer_.begin() and *temp not_eq '\n') {
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



void TextEditorModule::paste_selection(Vector<char>& source)
{
    if (state_->sel_begin_) {
        delete_selection();
    }

    auto insert = insert_pos();

    for (char c : source) {
        PLATFORM_EXTENSION(feed_watchdog);
        insert_char(c, insert);
        if (c == '\n') {
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



void TextEditorModule::save_selection(Vector<char>& output)
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
        output.push_back(*begin);
        ++begin;
    }

    output.push_back(*end);
}



void TextEditorModule::insert_char(char c,
                                   Optional<Vector<char>::Iterator> hint)
{
    state_->modified_ = true;

    if (c == '\n') {
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
