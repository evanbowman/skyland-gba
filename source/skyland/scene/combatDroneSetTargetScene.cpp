////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


#include "combatDroneSetTargetScene.hpp"
#include "skyland/network.hpp"
#include "skyland/scene/inspectP2Scene.hpp"
#include "skyland/scene/readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr<Scene> CombatDroneSetTargetScene::update(Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(delta)) {
        return new_scene;
    }

    if (not APP.opponent_island()) {
        return scene_pool::alloc<ReadyScene>();
    }

    auto exit_scene = [&]() -> ScenePtr<Scene> {
        if (is_player_island(drone_->destination())) {
            globals().near_cursor_loc_ = drone_->position();
            return scene_pool::alloc<ReadyScene>();
        } else {
            globals().far_cursor_loc_ = drone_->position();
            return scene_pool::alloc<InspectP2Scene>();
        }
    };

    if (targets_.empty()) {
        return exit_scene();
    }

    if (APP.player().key_down(Key::action_2)) {
        return exit_scene();
    }

    if (APP.player().key_down(Key::action_1)) {

        network::packet::DroneSetTarget packet;
        packet.drone_x_ = drone_->position().x;
        packet.drone_y_ = drone_->position().y;
        packet.target_x_ = cursor_loc_.x;
        packet.target_y_ = cursor_loc_.y;
        packet.drone_near_ = is_player_island(drone_->destination());
        packet.target_near_ = near_;
        network::transmit(packet);

        drone_->set_target(cursor_loc_, near_);

        return exit_scene();
    }

    if (APP.player().key_down(Key::right)) {
        ++selector_;
        if (selector_ >= (int)targets_.size()) {
            selector_ = 0;
        }
    }

    if (APP.player().key_down(Key::left)) {
        --selector_;
        if (selector_ < 0) {
            selector_ = targets_.size() - 1;
        }
    }

    auto target = targets_[selector_];
    auto loc = target->position();
    cursor_loc_ = loc;

    if (is_player_island(target->destination())) {
        near_camera();
        globals().near_cursor_loc_ = loc;
        near_ = true;
    } else {
        far_camera();
        globals().far_cursor_loc_ = loc;
        near_ = false;
    }

    return null_scene();
}



void CombatDroneSetTargetScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    auto collect = [&](auto& list) {
        for (auto& drone_sp : list) {
            if (drone_sp.get() == drone_.get()) {
                continue;
            }
            if (drone_sp->parent() not_eq drone_->parent()) {
                targets_.emplace_back(drone_sp);
            }
        }
    };


    near_ = is_player_island(drone_->destination());
    if (not near_) {
        far_camera();
    }

    if (near_) {
        if (APP.opponent_island()) {
            collect(APP.opponent_island()->drones());
        }
        collect(APP.player_island().drones());
    } else {
        collect(APP.player_island().drones());
        if (APP.opponent_island()) {
            collect(APP.opponent_island()->drones());
        }
    }
}



void CombatDroneSetTargetScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);
}



void CombatDroneSetTargetScene::display()
{
    if (targets_.empty()) {
        WorldScene::display();
        return;
    }

    if (not APP.opponent_island()) {
        WorldScene::display();
        return;
    }

    Island* island;
    if (near_) {
        island = &APP.player_island();
    } else {
        island = APP.opponent_island();
    }

    if (island) {
        auto origin = island->visual_origin();

        origin.x += Fixnum::from_integer(cursor_loc_.x * 16);
        origin.y += Fixnum::from_integer(cursor_loc_.y * 16);

        Sprite sprite;
        sprite.set_position(origin);
        sprite.set_tidx_16x16(17, 0);
        sprite.set_size(Sprite::Size::w16_h16);

        PLATFORM.screen().draw(sprite);
    }

    WorldScene::display();
}



} // namespace skyland
