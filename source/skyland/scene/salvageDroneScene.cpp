////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



void SalvageDroneScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

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



void SalvageDroneScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    yes_text_.reset();
    no_text_.reset();
    text_.reset();

    PLATFORM.fill_overlay(0);
}



ScenePtr SalvageDroneScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (APP.player().key_down(Key::action_2)) {
        return make_scene<ReadyScene>();
    }

    if (APP.player().key_down(Key::action_1)) {
        for (auto& room : drone_->parent()->rooms()) {
            auto found = room->drone();
            if (found and (*found).get() == drone_.get()) {
                room->detach_drone(false);

                network::packet::DroneDestroyed destroyed;
                destroyed.drone_x_ = drone_->position().x;
                destroyed.drone_y_ = drone_->position().y;
                destroyed.destination_near_ =
                    is_player_island(drone_->destination());

                network::transmit(destroyed);

                break;
            }
        }
        return make_scene<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
