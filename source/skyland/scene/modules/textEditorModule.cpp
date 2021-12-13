#include "textEditorModule.hpp"
#include "localization.hpp"
#include "skyland/skyland.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/scene/titleScreenScene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



static const int y_max = 19;



void TextEditorModule::show_status(Platform& pfrm)
{
    const auto status_colors = FontColors{
        custom_color(0x000010), custom_color(0xffffff)
    };

    status_->assign("line ", status_colors);
    status_->append(cursor_.y + 1, status_colors);
    status_->append("/", status_colors);
    status_->append(line_count_ + 1, status_colors);
    status_->append(" col ", status_colors);
    status_->append(cursor_.x + 1, status_colors);

    while (status_->len() not_eq 30) {
        status_->append(" ", status_colors);
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
                ++cursor_.x;
                ideal_cursor_right_ = cursor_.x;
                if (cursor_.x > column_offset_ + 29) {
                    ++column_offset_;
                }
                render(pfrm, start_line_);
                shade_cursor();
                show_status(pfrm);
            }
        } else if (app.player().key_down(pfrm, Key::left) and cursor_.x > 0) {
            unshade_cursor();
            cursor_flicker_timer_ = -seconds(1);
            --cursor_.x;
            ideal_cursor_right_ = cursor_.x;
            if (cursor_.x < column_offset_) {
                --column_offset_;
            }

            render(pfrm, start_line_);
            shade_cursor();
            show_status(pfrm);
        } else if (app.player().key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        break;

    case Mode::edit:
        break;
    }

    return null_scene();
}



TextEditorModule::Factory TextEditorModule::factory_;



}
