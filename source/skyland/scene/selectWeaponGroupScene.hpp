////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



    ScenePtr update(Time delta) override
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
    Optional<Text> text_;
};



} // namespace skyland
