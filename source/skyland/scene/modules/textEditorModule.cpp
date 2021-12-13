#include "textEditorModule.hpp"
#include "localization.hpp"
#include "skyland/skyland.hpp"
#include "platform/ram_filesystem.hpp"
#include "skyland/scene/titleScreenScene.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



static const int y_max = 19;



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
            ++y;
            x = 0;
            skipped = 0;
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

    const auto status_colors = FontColors{
        custom_color(0x000010), custom_color(0xffffff)
    };

    status_->assign("line ", status_colors);
    status_->append(start_line_ + 1, status_colors);
    status_->append("/", status_colors);
    status_->append(line_count_ + 1, status_colors);
    status_->append(" col ", status_colors);
    status_->append(column_offset_ + 1, status_colors);

    while (status_->len() not_eq 30) {
        status_->append(" ", status_colors);
    }
}



TextEditorModule::TextEditorModule(Platform& pfrm, const char* ram_file_path)
{
    text_buffer_ = pfrm.make_scratch_buffer();

    ram_filesystem::read_file_data(pfrm, ram_file_path, *text_buffer_);
}



const char* test_file = ";;\n;; file.lisp\n;;\n\n(print \"hello!\")\n;; a comment";



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
            if (*data == '\n') {
                ++line_count_;
            }
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
    switch (mode_) {
    case Mode::explore:
        if (app.player().key_down(pfrm, Key::up) and start_line_ > 0) {
            --start_line_;
            render(pfrm, start_line_);
        } else if (app.player().key_down(pfrm, Key::down)) {
            ++start_line_;
            render(pfrm, start_line_);
        } else if (app.player().key_down(pfrm, Key::right)) {
            ++column_offset_;
            render(pfrm, start_line_);
        } else if (app.player().key_down(pfrm, Key::left) and column_offset_ > 0) {
            --column_offset_;
            render(pfrm, start_line_);
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
