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

#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectWeaponGroupScene : public ActiveWorldScene
{
public:
    SelectWeaponGroupScene(DeferredScene cancel) : cancel_(cancel)
    {
    }


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        text_.emplace(SYSTR(modifier_keys_opt_5)->c_str(),
                      OverlayCoord{0, u8(calc_screen_tiles().y - 1)});

        text_->append(" ");

        PLATFORM.set_tile(
            Layer::overlay, text_->len(), calc_screen_tiles().y - 1, 395);

        PLATFORM.set_tile(
            Layer::overlay, text_->len() + 1, calc_screen_tiles().y - 1, 393);

        PLATFORM.set_tile(
            Layer::overlay, text_->len() + 2, calc_screen_tiles().y - 1, 394);

        for (int i = 0; i < text_->len() + 3; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 18, 425);
        }
    }



    void exit(Scene& prev) override
    {
        ActiveWorldScene::exit(prev);

        text_.reset();

        PLATFORM.fill_overlay(0);
    }



    ScenePtr<Scene> update(Microseconds delta) override
    {
        if (auto next = ActiveWorldScene::update(delta)) {
            return next;
        }

        if (APP.player().key_down(Key::up)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::one) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::right)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::two) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::left)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::three) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::action_2)) {
            return cancel_();
        }

        return null_scene();
    }

private:
    DeferredScene cancel_;
    std::optional<Text> text_;
};



} // namespace skyland
