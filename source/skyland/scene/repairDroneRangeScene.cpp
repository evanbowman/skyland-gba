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


#include "repairDroneRangeScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



ScenePtr<Scene> RepairDroneRangeScene::update(Microseconds delta)
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
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
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
