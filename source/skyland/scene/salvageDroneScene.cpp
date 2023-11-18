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


#include "salvageDroneScene.hpp"
#include "readyScene.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/network.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void SalvageDroneScene::enter(App& app, Scene& prev)
{
    ActiveWorldScene::enter(app, prev);

    auto st = calc_screen_tiles();
    StringBuffer<30> text(SYSTR(salvage_drone)->c_str());

    text_.emplace(text.c_str(), OverlayCoord{0, u8(st.y - 1)});
    for (int i = 0; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
    }

    yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
    no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

    yes_text_->assign(SYSTR(salvage_option_A)->c_str());
    no_text_->assign(SYSTR(salvage_option_B)->c_str());

    const int count = st.x - text_->len();
    for (int i = 0; i < count; ++i) {
        PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
    }

    for (int i = 23; i < st.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
    PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);
}



void SalvageDroneScene::exit(App& app, Scene& next)
{
    ActiveWorldScene::exit(app, next);

    yes_text_.reset();
    no_text_.reset();
    text_.reset();

    PLATFORM.fill_overlay(0);
}



ScenePtr<Scene> SalvageDroneScene::update(App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(app, delta)) {
        return new_scene;
    }

    if (app.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
    }

    if (app.player().key_down(Key::action_1)) {
        for (auto& room : drone_->parent()->rooms()) {
            auto found = room->drone();
            if (found and (*found).get() == drone_.get()) {
                room->detach_drone(app, false);

                network::packet::DroneDestroyed destroyed;
                destroyed.drone_x_ = drone_->position().x;
                destroyed.drone_y_ = drone_->position().y;
                destroyed.destination_near_ =
                    drone_->destination() == &app.player_island();

                network::transmit(destroyed);

                break;
            }
        }
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
