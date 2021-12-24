#include "createFileScene.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "modules/textEditorModule.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland {



CreateFileScene::CreateFileScene(const char* ram_file_path) :
    file_path_(ram_file_path)
{
}


static const char* keyboard[7][7] = {{"z", "y", "g", "f", "v", "q", ";"},
                                     {"m", "b", "i", "d", "l", "j", "\""},
                                     {"w", "a", "o", "e", "u", "k", "/"},
                                     {"p", "h", "t", "n", "s", "r", "_"},
                                     {"x", "c", "(", ")", "-", " ", "."},
                                     {"$", "'", "0", "1", "2", "3", "X"},
                                     {"4", "5", "6", "7", "8", "9", "\n"}};


ScenePtr<Scene> CreateFileScene::update(Platform& pfrm,
                                        App& app,
                                        Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::left)) {
        if (keyboard_cursor_.x > 0) {
            --keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = 6;
        }
        render_keyboard(pfrm);
    } else if (app.player().key_down(pfrm, Key::right)) {
        if (keyboard_cursor_.x < 6) {
            ++keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = 0;
        }
        render_keyboard(pfrm);
    } else if (app.player().key_down(pfrm, Key::up)) {
        if (keyboard_cursor_.y > 0) {
            --keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 6;
        }
        render_keyboard(pfrm);
    } else if (app.player().key_down(pfrm, Key::down)) {
        if (keyboard_cursor_.y < 6) {
            ++keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 0;
        }
        render_keyboard(pfrm);
    } else if (app.player().key_down(pfrm, Key::action_1)) {
        const char c = keyboard[keyboard_cursor_.y][keyboard_cursor_.x][0];
        path_.push_back(c);
        entry_->assign(path_.c_str());
    } else if (app.player().key_down(pfrm, Key::action_2)) {
        if (not path_.empty()) {
            path_.pop_back();
            entry_->assign(path_.c_str());
        } else {
            // TODO: exit
        }
    } else if (app.player().key_down(pfrm, Key::start)) {
        if (not path_.empty()) {
            StringBuffer<100> full_path_(file_path_.c_str());
            full_path_ += path_;

            return scene_pool::alloc<TextEditorModule>(pfrm,
                                                       full_path_.c_str(),
                                                       TextEditorModule::FileMode::create);
        }
    }

    return null_scene();
}


static const auto status_colors = FontColors{
    custom_color(0x000010), custom_color(0xffffff)
};


void CreateFileScene::render_keyboard(Platform& pfrm)
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


void CreateFileScene::enter(Platform& pfrm,
                            App& app,
                            Scene& prev)
{
    render_keyboard(pfrm);

    title_text_.emplace(pfrm, "create file:", OverlayCoord{1, 1});
    entry_.emplace(pfrm, OverlayCoord{1, 4});
}



void CreateFileScene::exit(Platform& pfrm,
                           App& app,
                           Scene& next)
{
    title_text_.reset();
    entry_.reset();

    pfrm.fill_overlay(0);
}



}
