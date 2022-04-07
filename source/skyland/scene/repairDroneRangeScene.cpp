////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
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



ScenePtr<Scene>
RepairDroneRangeScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (drone_->destination() not_eq &app.player_island()) {
        far_camera();
    }

    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
        return scene;
    }

    if (not description_) {
        description_.emplace(
            pfrm, SYSTR(repair_range)->c_str(), OverlayCoord{0, 19});

        for (int i = 0; i < description_->len(); ++i) {
            pfrm.set_tile(Layer::overlay, i, 18, 425);
        }
    }

    if (app.player().key_down(pfrm, Key::action_2)) {
        description_.reset();
        pfrm.fill_overlay(0);

        if (is_far_camera()) {
            return scene_pool::alloc<InspectP2Scene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }
    }

    return null_scene();
}



void RepairDroneRangeScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    auto pos = drone_->position();

    auto origin = drone_->destination()->visual_origin();

    Sprite sprite;
    sprite.set_size(Sprite::Size::w16_h32);
    sprite.set_texture_index(13);

    for (int x = pos.x - 2; x < pos.x + 3; ++x) {
        sprite.set_position({origin.x + x * 16, origin.y + (pos.y - 2) * 16});
        pfrm.screen().draw(sprite);


        if (x not_eq pos.x) {
            sprite.set_texture_index(14);

            sprite.set_position(
                {origin.x + x * 16, origin.y + (pos.y - 1) * 16});

            pfrm.screen().draw(sprite);

            sprite.set_texture_index(13);
        }


        sprite.set_position({origin.x + x * 16, origin.y + (pos.y + 1) * 16});

        pfrm.screen().draw(sprite);
    }
}



} // namespace skyland
