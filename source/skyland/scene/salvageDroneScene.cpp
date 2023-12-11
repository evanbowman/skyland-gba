////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



ScenePtr<Scene> SalvageDroneScene::update(Time delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (APP.player().key_down(Key::action_2)) {
        return scene_pool::alloc<ReadyScene>();
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
        return scene_pool::alloc<ReadyScene>();
    }

    return null_scene();
}



} // namespace skyland
