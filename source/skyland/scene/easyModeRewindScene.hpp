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


#pragma once

#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"


namespace skyland
{



class EasyModeRewindScene : public WorldScene
{
public:
    void enter(Scene& prev) override
    {
        WorldScene::enter(prev);

        PLATFORM.load_overlay_texture("overlay_challenges");

        yes_text_.emplace(OverlayCoord{3, 7});
        no_text_.emplace(OverlayCoord{3, 9});

        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(no)->c_str());

        auto title_str = SYSTR(easy_mode_auto_rewind_title);
        u8 mg = centered_text_margins(utf8::len(title_str->c_str()));

        title_.emplace(title_str->c_str(), OverlayCoord{mg, 1});

        auto lives = SYSTR(easy_mode_auto_rewind_text);

        text_.emplace(

            format(lives->c_str(), APP.persistent_data().lives_ + 1).c_str(),
            OverlayCoord{1, 5});

        PLATFORM.screen().pixelate(128, false);
        PLATFORM.screen().schedule_fade(0.7f);
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.load_overlay_texture("overlay");
        WorldScene::exit(next);

        title_.reset();
        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        PLATFORM.screen().pixelate(0, false);
        PLATFORM.screen().schedule_fade(0.f);
    }


    ScenePtr<Scene> update(Microseconds delta) override
    {
        if (APP.player().key_down(Key::up)) {
            if (selected_ not_eq 0) {
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 0;
        } else if (APP.player().key_down(Key::down)) {
            if (selected_ not_eq 1) {
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 1;
        }

        if (APP.player().key_down(Key::action_1)) {
            switch (selected_) {
            case 0:
                state_bit_store(StateBit::easy_mode_rewind_declined, false);
                return scene_pool::alloc<RewindScene>(false);

            case 1:
                state_bit_store(StateBit::easy_mode_rewind_declined, true);
                return scene_pool::alloc<ReadyScene>();
            }
        }



        // NOTE: because the procgen ai forcibly sets level number in top
        // corner.
        PLATFORM.set_tile(Layer::overlay, calc_screen_tiles().x - 1, 0, 0);
        PLATFORM.set_tile(Layer::overlay, calc_screen_tiles().x - 2, 0, 0);

        if (selected_ == 0) {
            PLATFORM.set_tile(Layer::overlay, 1, yes_text_->coord().y, 87);
            PLATFORM.set_tile(Layer::overlay, 1, no_text_->coord().y, 0);
        } else {
            PLATFORM.set_tile(Layer::overlay, 1, yes_text_->coord().y, 0);
            PLATFORM.set_tile(Layer::overlay, 1, no_text_->coord().y, 87);
        }


        return null_scene();
    }

private:
    std::optional<Text> title_;
    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
    std::optional<Text> lives_;

    int selected_ = 0;
};



} // namespace skyland
