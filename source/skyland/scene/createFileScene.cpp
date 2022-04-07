////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "createFileScene.hpp"
#include "modules/textEditorModule.hpp"
#include "paintScene.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



CreateFileScene::CreateFileScene(const char* ram_file_path)
    : file_path_(ram_file_path)
{
}


static const char* keyboard[7][7] = {{"z", "y", "g", "f", "v", "q", ";"},
                                     {"m", "b", "i", "d", "l", "j", "\""},
                                     {"w", "a", "o", "e", "u", "k", "/"},
                                     {"p", "h", "t", "n", "s", "r", "_"},
                                     {"x", "c", "(", ")", "-", " ", "."},
                                     {"$", "'", "0", "1", "2", "3", "X"},
                                     {"4", "5", "6", "7", "8", "9", "\n"}};



static const FontColors text_entry_colors{custom_color(0xffffff),
                                          custom_color(0x181835)};



TextEditorModule::SyntaxMode file_edit_mode(const StringBuffer<200>& path);



StringBuffer<16> get_extension(const StringBuffer<200>& cwd);



ScenePtr<Scene>
CreateFileScene::update(Platform& pfrm, App& app, Microseconds delta)
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
        auto temp = path_;
        while (not temp.full()) {
            temp.push_back(' ');
        }
        entry_->assign(temp.c_str(), text_entry_colors);

    } else if (app.player().key_down(pfrm, Key::action_2)) {
        if (not path_.empty()) {
            path_.pop_back();
            auto temp = path_;
            while (not temp.full()) {
                temp.push_back(' ');
            }
            entry_->assign(temp.c_str(), text_entry_colors);
        } else {
            // TODO: exit
        }
    } else if (app.player().key_down(pfrm, Key::start)) {
        if (not path_.empty()) {
            StringBuffer<100> full_path_(file_path_.c_str());
            full_path_ += path_;

            if (get_extension(full_path_) == ".img") {
                return scene_pool::alloc<PaintScene>(full_path_.c_str(), true);
            } else {
                UserContext ctx;

                return scene_pool::alloc<TextEditorModule>(
                    pfrm,
                    std::move(ctx),
                    full_path_.c_str(),
                    file_edit_mode(full_path_),
                    TextEditorModule::FileMode::create);
            }
        }
    }

    return null_scene();
}


static const auto status_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};


void CreateFileScene::render_keyboard(Platform& pfrm)
{
    for (int x = 0; x < 7; ++x) {
        for (int y = 0; y < 7; ++y) {
            const char c = keyboard[y][x][0];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = pfrm.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if (x == keyboard_cursor_.x and y == keyboard_cursor_.y) {
                colors = FontColors{custom_color(0xffffff),
                                    ColorConstant::aerospace_orange};
            }

            pfrm.set_tile((30 - 8) + x, (19 - 6) + y, t, colors);
        }
    }
}


void CreateFileScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    render_keyboard(pfrm);

    title_text_.emplace(pfrm, "create file:", OverlayCoord{1, 1});
    entry_.emplace(pfrm, OverlayCoord{1, 4});
    entry_->assign(StringBuffer<28>(' ', 28).c_str(), text_entry_colors);
}



void CreateFileScene::exit(Platform& pfrm, App& app, Scene& next)
{
    title_text_.reset();
    entry_.reset();

    pfrm.fill_overlay(0);
}



} // namespace skyland
