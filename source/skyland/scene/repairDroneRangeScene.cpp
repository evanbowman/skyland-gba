////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "repairDroneRangeScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



ScenePtr RepairDroneRangeScene::update(Time delta)
{
    if (drone_->destination() not_eq &APP.player_island()) {
        far_camera();
    }

    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    if (not description_) {
        description_.emplace(SYSTR(repair_range)->c_str(), OverlayCoord{0, 19});

        for (int i = 0; i < description_->len(); ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 18, 425);
        }
    }

    if (APP.player().key_down(Key::action_2)) {
        description_.reset();
        PLATFORM.fill_overlay(0);

        if (is_far_camera()) {
            return make_scene<InspectP2Scene>();
        } else {
            return make_scene<ReadyScene>();
        }
    }

    return null_scene();
}



void RepairDroneRangeScene::display()
{
    WorldScene::display();

    auto pos = drone_->position();

    auto origin = drone_->destination()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y - 2) * 16)});
        PLATFORM.screen().draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_size(Sprite::Size::w16_h16);
            sprite.set_tidx_16x16(13, 1);

            sprite.set_position(
                {origin.x + Fixnum::from_integer(x * 16),
                 origin.y + Fixnum::from_integer((pos.y) * 16)});

            PLATFORM.screen().draw(sprite);
            sprite.set_size(Sprite::Size::w16_h32);
            sprite.set_texture_index(13);
        }


        sprite.set_position(
            {origin.x + Fixnum::from_integer(x * 16),
             origin.y + Fixnum::from_integer((pos.y + 1) * 16)});

        PLATFORM.screen().draw(sprite);
    }
}



} // namespace skyland
