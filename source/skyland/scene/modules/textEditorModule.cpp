#include "textEditorModule.hpp"
#include "localization.hpp"
#include "skyland/skyland.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/scene/titleScreenScene.hpp"



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
        status_->assign("line ", status_colors);
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

    while (*data not_eq '\0' and y not_eq y_max) {

        if (x == 30) {
            while (*data not_eq '\n') {
                if (*data == '\0') {
                    goto FILL;
                }
                ++data;
            }
            ++data;
            ++y;
            x = 0;
            skipped = 0;
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

            x = 0;
            skipped = 0;

            ++data;
            ++y;

            continue;
        }

        if (skipped < column_offset_) {
            ++skipped;
            ++data;
            continue;
        }

        const char c = *data;

        auto mapping_info = locale_texture_map()(c);

        if (mapping_info) {
            u16 t = pfrm.map_glyph(c, *mapping_info);
            pfrm.set_tile(Layer::overlay, x, y, t);
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
    header_->assign("  text editor                 ", FontColors{
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

        if (app.player().key_down(pfrm, Key::up) and cursor_.y > 0) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            --cursor_.y;

            bool do_render = true;

            if (cursor_.y < start_line_) {
                --start_line_;
                do_render = true;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                do_render = true;
            }

            while (cursor_.x > column_offset_ + 29) {
                ++column_offset_;
                do_render = true;
            }

            if (do_render) {
                render(pfrm, start_line_);
            }

            shade_cursor();
            show_status(pfrm);
        } else if (app.player().key_down(pfrm, Key::down) and cursor_.y < line_count_) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            ++cursor_.y;

            bool do_render = false;

            if (cursor_.y > start_line_ + 17) {
                ++start_line_;
                do_render = true;
            }
            cursor_.x = ideal_cursor_right_;

            const auto len = line_length();
            if (cursor_.x > len) {
                cursor_.x = len;
                if (cursor_.x < column_offset_) {
                    column_offset_ = cursor_.x;
                }
                do_render = true;
            }

            while (cursor_.x > column_offset_ + 29) {
                ++column_offset_;
                do_render = true;
            }

            if (do_render) {
                render(pfrm, start_line_);
            }

            shade_cursor();
            show_status(pfrm);
        } else if (app.player().key_down(pfrm, Key::right)) {
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
        } else if (app.player().key_down(pfrm, Key::left) and cursor_.x > 0) {
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
                char c = keyboard[keyboard_cursor_.y][keyboard_cursor_.x][0];
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
            }
            render(pfrm, start_line_);
            shade_cursor();
        }
        break;

    case Mode::autocomplete:
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



TextEditorModule::Factory TextEditorModule::factory_;



}
