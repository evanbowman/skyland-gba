#include "textEditorModule.hpp"
#include "localization.hpp"
#include "skyland/skyland.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "script/lisp.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



static const int y_max = 19;



static const auto status_colors = FontColors{
    custom_color(0x000010), custom_color(0xffffff)
};



void TextEditorModule::show_status(Platform& pfrm)
{
    if (mode_ == Mode::autocomplete) {
        return;
    }

    if (mode_ == Mode::edit) {
        status_->assign("edit: ", status_colors);
    } else {
        status_->assign("nav: line ", status_colors);
        status_->append(cursor_.y + 1, status_colors);
        status_->append("/", status_colors);
        status_->append(line_count_ + 1, status_colors);
        status_->append(" col ", status_colors);
        status_->append(cursor_.x + 1, status_colors);
    }

    while (status_->len() not_eq 30) {
        status_->append(" ", status_colors);
    }

}



static const char* keyboard[7][7] = {{"z", "y", "g", "f", "v", "q", ";"},
                                     {"m", "b", "i", "d", "l", "j", "\""},
                                     {"w", "a", "o", "e", "u", "k", "/"},
                                     {"p", "h", "t", "n", "s", "r", "_"},
                                     {"x", "c", "(", ")", "-", " ", "."},
                                     {"$", "'", "0", "1", "2", "3", "\n"},
                                     {"4", "5", "6", "7", "8", "9", "_"}};



void TextEditorModule::render_keyboard(Platform& pfrm)
{
    for (int x = 0; x < 7; ++x) {
        for (int y = 0; y < 7; ++y) {
            const char c = keyboard[y][x][0];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = pfrm.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if (x == keyboard_cursor_.x and y == keyboard_cursor_.y) {
                colors = FontColors{
                    custom_color(0xffffff), ColorConstant::aerospace_orange
                };
            }

            pfrm.set_tile((30 - 8) + x, (19 - 6) + y, t, colors);
        }
    }
}



void TextEditorModule::render_completions(Platform& pfrm)
{
    char c = ' ';
    const u16 space = pfrm.map_glyph(c, *locale_texture_map()(c));

    int line = 13;
    for (auto& cpl : completions_) {

        auto colors = status_colors;
        if (line - 13 == selected_completion_) {
            colors = FontColors{
                custom_color(0xffffff), ColorConstant::aerospace_orange
            };
        }

        u32 x;
        for (x = 0; x < 30 and x < cpl.length(); ++x) {
            char c = cpl[x];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = pfrm.map_glyph(c, *mapping_info);

            auto prefix_colors = colors;

            if (x < current_word_.length() and line - 13 not_eq selected_completion_) {
                prefix_colors.foreground_ = custom_color(0x4646d2);
            }

            pfrm.set_tile(x, line, t, prefix_colors);
        }

        for (; x < 30; ++x) {
            pfrm.set_tile(x, line, space, colors);
        }

        ++line;
    }

    for (; line < 20; ++line) {
        for (int x = 0; x < 30; ++x) {
            pfrm.set_tile(x, line, space, status_colors);
        }
    }
}



void TextEditorModule::render(Platform& pfrm, int start_line)
{
    int x = 0;
    int y = 1;

    const char* data = (*text_buffer_)->data_;

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

    bool comment = false;
    bool quotation = false;
    bool endquote = false;
    bool keyword = false;

    auto handle_char = [&](char c) {
        if (c == ';') {
            comment = true;
        } else if (c == '"') {
            if (not quotation) {
                quotation = true;
            } else {
                endquote = true;
            }
        } else if (c == ' ' or c == ')' or c == '(') {
            keyword = false;

            auto seek = data + 1;
            StringBuffer<32> word;

            while (*seek not_eq '\0' and
                   *seek not_eq ' ' and
                   *seek not_eq '(' and
                   *seek not_eq ')' and
                   *seek not_eq '\n') {
                word.push_back(*seek);
                ++seek;
            }

            if (word == "def" or
                word == "defn/c" or
                word == "let" or
                word == "lambda" or
                word == "if" or
                word == "or" or
                word == "and" or
                word == "cond" or
                word == "progn") {
                keyword = true;
            }
        }
    };

    while (*data not_eq '\0' and y not_eq y_max) {

        endquote = false;

        auto on_newline = [&] {
            x = 0;
            skipped = 0;
            comment = false;
            quotation = false;
        };

        if (x == 30) {
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

        if (*data == '\n') {

            const char c = ' ';

            auto mapping_info = locale_texture_map()(c);
            const u16 t = pfrm.map_glyph(c, *mapping_info);

            while (x not_eq 30) {
                pfrm.set_tile(Layer::overlay, x, y, t);
                ++x;
            }

            on_newline();

            ++data;
            ++y;

            continue;
        }

        if (skipped < column_offset_) {
            ++skipped;
            handle_char(*data);
            ++data;
            continue;
        }

        const char c = *data;

        handle_char(c);

        auto mapping_info = locale_texture_map()(c);

        if (mapping_info) {
            u16 t = pfrm.map_glyph(c, *mapping_info);
            if (comment or quotation) {
                pfrm.set_tile(x, y, t, FontColors{
                        custom_color(0x6eb98e), custom_color(0x110731)
                    });
            } else if (keyword and c not_eq '(') {
                pfrm.set_tile(x, y, t, FontColors{
                        custom_color(0xde2f79), custom_color(0x110731)
                    });
            } else {
                pfrm.set_tile(Layer::overlay, x, y, t);
            }
        }

        if (endquote) {
            quotation = false;
        }


        ++data;
        ++x;
    }

 FILL:

    const char c = ' ';

    auto mapping_info = locale_texture_map()(c);
    const u16 t = pfrm.map_glyph(c, *mapping_info);

    while (y not_eq y_max) {
        while (x not_eq 30) {
            pfrm.set_tile(Layer::overlay, x, y, t);
            ++x;
        }
        ++y;
        x = 0;
    }

    show_status(pfrm);

    if (show_keyboard_) {
        render_keyboard(pfrm);
    }

    if (show_completions_) {
        render_completions(pfrm);
    }
}



StringBuffer<32> TextEditorModule::current_word()
{
    auto begin = (*text_buffer_)->data_;
    auto data = insert_pos();

    --data;

    auto is_delim = [](char c)
    {
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
    if (count == 0) {
        count = 1;
    }
    return count;
}



int TextEditorModule::back_word()
{
    auto begin = (*text_buffer_)->data_;

    auto data = insert_pos();

    int count = 0;
    while (data not_eq begin and
           *data not_eq '\0' and *data not_eq '\n' and *data not_eq ' ') {
        ++count;
        --data;
    }
    if (count == 0) {
        count = 1;
    }
    return count;
}



const char* TextEditorModule::current_line() const
{
    const char* data = (*text_buffer_)->data_;

    int line = 0;
    while (*data not_eq '\0' and line not_eq cursor_.y) {
        if (*data == '\n') {
            ++line;
        }

        ++data;
    }

    return data;
}



int TextEditorModule::line_length() const
{
    auto data = current_line();

    int length = 0;

    while (*data not_eq '\0' and *data not_eq '\n') {
        ++length;
        ++data;
    }

    return length;
}



TextEditorModule::TextEditorModule(Platform& pfrm, const char* ram_file_path)
{
    text_buffer_ = pfrm.make_scratch_buffer();

    ram_filesystem::read_file_data(pfrm, ram_file_path, *text_buffer_);
}



const char* test_file = ";;;\n"
    ";;; init.lisp\n"
    ";;;\n"
    "\n"
    "\n"
    "(eval-other-file \"stdlib.lisp\")\n"
    "\n"
    "\n"
    "(def language 'english)\n"
    "\n"
    "(defn/c locale-string\n"
    "  (get-line-of-file (string \"strings/\" language '.txt) $0))\n"
    "";



void TextEditorModule::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_editor");

    header_.emplace(pfrm, OverlayCoord{});
    header_->assign("  text editor  (lisp mode)    ", FontColors{
            custom_color(0x000010), custom_color(0xffffff)
        });


    status_.emplace(pfrm, OverlayCoord{0, 19});


    if (not text_buffer_) {
        text_buffer_ = pfrm.make_scratch_buffer();

        auto data = test_file;
        int i = 0;
        while (*data not_eq '\0') {
            (*text_buffer_)->data_[i++] = *(data++);
        }
    }

    auto data = (*text_buffer_)->data_;
    while (*data not_eq '\0') {
        if (*data == '\n') {
            ++line_count_;
        }
        ++data;
    }

    render(pfrm, 0);

    pfrm.screen().fade(0.f);

}



void TextEditorModule::exit(Platform& pfrm, App&, Scene& next)
{
    header_.reset();
    status_.reset();

    pfrm.fill_overlay(0);
}



ScenePtr<Scene> TextEditorModule::update(Platform& pfrm,
                                         App& app,
                                         Microseconds delta)
{
    auto unshade_cursor = [&] {
        cursor_shaded_ = false;

        const auto x = cursor_.x - column_offset_;
        const auto y = (cursor_.y - start_line_) + 1;

        const auto t = pfrm.get_tile(Layer::overlay, x, y);
        pfrm.set_tile(Layer::overlay, x, y, t);
    };

    auto shade_cursor = [&] {
        cursor_shaded_ = true;

        const auto x = cursor_.x - column_offset_;
        const auto y = (cursor_.y - start_line_) + 1;

        static const auto highlight_colors = FontColors{
            custom_color(0x000010), ColorConstant::aerospace_orange
        };

        const auto t = pfrm.get_tile(Layer::overlay, x, y);
        pfrm.set_tile(x, y, t, highlight_colors);
    };

    switch (mode_) {
    case Mode::explore:

        cursor_flicker_timer_ += delta;
        if (cursor_flicker_timer_ > milliseconds(200)) {
            cursor_flicker_timer_ = 0;

            if (cursor_shaded_) {
                unshade_cursor();
            } else {
                shade_cursor();
            }
        }

        if (app.player().key_pressed(pfrm, Key::up)) {
            key_held_timer_[0] += delta;
        } else {
            key_held_timer_[0] = 0;
        }

        if (app.player().key_pressed(pfrm, Key::down)) {
            key_held_timer_[1] += delta;
        } else {
            key_held_timer_[1] = 0;
        }

        if (app.player().key_pressed(pfrm, Key::left)) {
            key_held_timer_[2] += delta;
        } else {
            key_held_timer_[2] = 0;
        }

        if (app.player().key_pressed(pfrm, Key::right)) {
            key_held_timer_[3] += delta;
        } else {
            key_held_timer_[3] = 0;
        }

        if (app.player().key_pressed(pfrm, Key::alt_1)) {
            if (app.player().key_down(pfrm, Key::action_1)) {
                insert_char('\n');
                cursor_.x = 0;
                cursor_.y += 1;
                render(pfrm, start_line_);
                shade_cursor();
            } else if (app.player().key_down(pfrm, Key::action_2)) {
                // TODO: refactor this copy-pasted code
                erase_char();
                cursor_.x -= 1;
                if (cursor_.x == -1) {
                    cursor_.y -= 1;
                    cursor_.x = line_length();
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                while (cursor_.x > column_offset_ + 29) {
                    ++column_offset_;
                }
                render(pfrm, start_line_);
                shade_cursor();
            }
        } else if ((app.player().key_down(pfrm, Key::up) or
             (app.player().key_pressed(pfrm, Key::up) and
              key_held_timer_[0] > milliseconds(400))) and cursor_.y > 0) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            --cursor_.y;

            key_held_timer_[0] -= milliseconds(80);

            if (cursor_.y < start_line_) {
                --start_line_;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
            }

            while (cursor_.x > column_offset_ + 29) {
                ++column_offset_;
            }

            render(pfrm, start_line_);

            shade_cursor();
            show_status(pfrm);
        } else if ((app.player().key_down(pfrm, Key::down) or
                    (app.player().key_pressed(pfrm, Key::down) and
                     key_held_timer_[1] > milliseconds(400)))
                   and cursor_.y < line_count_) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            ++cursor_.y;

            key_held_timer_[1] -= milliseconds(80);

            if (cursor_.y > start_line_ + 17) {
                ++start_line_;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
            }

            while (cursor_.x > column_offset_ + 29) {
                ++column_offset_;
            }

            render(pfrm, start_line_);
            shade_cursor();
            show_status(pfrm);
        } else if (app.player().key_down(pfrm, Key::right) or
                   (app.player().key_pressed(pfrm, Key::right) and
                    key_held_timer_[3] > milliseconds(400))) {

            key_held_timer_[3] -= milliseconds(80);

            if (line_length() > cursor_.x) {
                unshade_cursor();
                cursor_flicker_timer_ = -seconds(1);
                // if (app.player().key_pressed(pfrm, Key::alt_1)) {
                //     cursor_.x += skip_word();
                // } else {
                    ++cursor_.x;
                // }

                ideal_cursor_right_ = cursor_.x;
                while (cursor_.x > column_offset_ + 29) {
                    ++column_offset_;
                }
                render(pfrm, start_line_);
                shade_cursor();
                show_status(pfrm);
            }
        } else if ((app.player().key_down(pfrm, Key::left) or
                    (app.player().key_pressed(pfrm, Key::left) and
                     key_held_timer_[2] > milliseconds(400)))
                   and cursor_.x > 0) {

            key_held_timer_[2] -= milliseconds(80);

            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            // if (app.player().key_pressed(pfrm, Key::alt_1)) {
            //     cursor_.x -= back_word();
            // } else {
                --cursor_.x;
            // }
            ideal_cursor_right_ = cursor_.x;
            while (cursor_.x < column_offset_) {
                --column_offset_;
            }
            render(pfrm, start_line_);
            shade_cursor();
            show_status(pfrm);
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<TitleScreenScene>();
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            start_line_ = std::max(0, cursor_.y - ((y_max - 2) / 2));
            show_keyboard_ = true;
            mode_ = Mode::edit;
            keyboard_cursor_ = {5, 4};
            render(pfrm, start_line_);
            shade_cursor();
        }
        break;

    case Mode::edit:
        if (app.player().key_down(pfrm, Key::action_2)) {
            mode_ = Mode::explore;
            show_keyboard_ = false;
            render(pfrm, start_line_);
            shade_cursor();
        } else if (app.player().key_down(pfrm, Key::left)) {
            if (keyboard_cursor_.x == 0) {
                keyboard_cursor_.x = 6;
            } else {
                --keyboard_cursor_.x;
            }
            render_keyboard(pfrm);
        } else if (app.player().key_down(pfrm, Key::right)) {
            if (keyboard_cursor_.x == 6) {
                keyboard_cursor_.x = 0;
            } else {
                ++keyboard_cursor_.x;
            }
            render_keyboard(pfrm);
        } else if (app.player().key_down(pfrm, Key::up)) {
            if (keyboard_cursor_.y == 0) {
                keyboard_cursor_.y = 6;
            } else {
                --keyboard_cursor_.y;
            }
            render_keyboard(pfrm);
        } else if (app.player().key_down(pfrm, Key::down)) {
            if (keyboard_cursor_.y == 6) {
                keyboard_cursor_.y = 0;
            } else {
                ++keyboard_cursor_.y;
            }
            render_keyboard(pfrm);
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            if (keyboard_cursor_.y == 6 and keyboard_cursor_.x == 6) {
                erase_char();
                cursor_.x -= 1;
                if (cursor_.x == -1) {
                    cursor_.y -= 1;
                    cursor_.x = line_length();
                }
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                while (cursor_.x > column_offset_ + 29) {
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

                    if (cursor_.x > column_offset_ + 29) {
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
            render(pfrm, start_line_);
            shade_cursor();
        } else if (app.player().key_down(pfrm, Key::alt_1)) {

            current_word_ = current_word();
            completions_.clear();

            // completions_.push_back(current_word_.c_str());

            lisp::get_interns([&](const char* intern) {
                if (completions_.full()) {
                    return;
                }

                const auto intern_len = str_len(intern);
                if (intern_len <= current_word_.length()) {
                    return;
                }

                for (u32 i = 0; i < current_word_.length(); ++i) {
                    if (current_word_[i] not_eq intern[i]) {
                        return;
                    }
                }

                completions_.push_back(intern);
            });

            mode_ = Mode::autocomplete;
            show_keyboard_ = false;
            show_completions_ = true;
            selected_completion_ = 0;
            render(pfrm, start_line_);
            shade_cursor();
        }
        break;

    case Mode::autocomplete:
        if (app.player().key_down(pfrm, Key::action_2)) {
            mode_ = Mode::edit;
            show_keyboard_ = true;
            show_completions_ = false;
            render(pfrm, start_line_);
            shade_cursor();
        } else if (app.player().key_down(pfrm, Key::up)) {
            if (selected_completion_ > 0) {
                --selected_completion_;
                render_completions(pfrm);
            }
        } else if (app.player().key_down(pfrm, Key::down)) {
            if (selected_completion_ < (int)completions_.size() - 1) {
                ++selected_completion_;
                render_completions(pfrm);
            }
        } else if (app.player().key_down(pfrm, Key::action_1)) {
            mode_ = Mode::edit;

            auto cpl = completions_[selected_completion_];
            for (u32 i = current_word_.length(); i < cpl.length(); ++i) {
                insert_char(cpl[i]);
                ++cursor_.x;
            }

            show_completions_ = false;
            show_keyboard_ = true;
            render(pfrm, start_line_);
            shade_cursor();
        }

        break;
    }

    return null_scene();
}



char* TextEditorModule::insert_pos()
{
    auto data = (*text_buffer_)->data_;

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



void TextEditorModule::erase_char()
{
    if (cursor_.x == 0 and cursor_.y == 0) {
        // Nothing left to delete.
        return;
    }

    auto begin = insert_pos() - 1;

    if (*begin == '\n') {
        --line_count_;
    }

    auto remaining = str_len(begin);
    auto end = begin + remaining;

    for (; begin not_eq end; ++begin) {
        if (begin + 1 not_eq end) {
            *begin = *(begin + 1);
        }
    }

    *end = '\0';
}



void TextEditorModule::insert_char(char c)
{
    const auto current_bytes = str_len((*text_buffer_)->data_);
    if (current_bytes == SCRATCH_BUFFER_SIZE - 1) {
        // TODO: raise error
        return;
    }

    if (c == '\n') {
        ++line_count_;
    }

    auto begin = insert_pos();

    // Bytes following the insert point that we'll need to shift over.
    auto remaining = str_len(begin);

    auto end = begin + remaining;

    for (auto it = end - 1; it not_eq begin - 1; --it) {
        *(it + 1) = *it;
    }

    *begin = c;
    *(end + 1) = '\0';
}



// TextEditorModule::Factory TextEditorModule::factory_;



}
