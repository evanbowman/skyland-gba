////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "multiplayerCoOpAwaitLockScene.hpp"



namespace skyland
{



ScenePtr MultiplayerCoOpAwaitLockScene::update(Time delta)
{
    auto on_failure = [&] {
        if (auto room = player_island().get_room(coord_)) {
            // Before entering this scene, the originating code should
            // have checked the room's lock and acquired it.
            room->co_op_peer_release_lock();
        }
        PLATFORM.speaker().play_sound("beep_error", 2);
        return make_scene<ReadyScene>();
    };

    if (auto scene = ActiveWorldScene::update(delta)) {
        on_failure();
        return scene;
    }

    if (result_) {
        if (*result_) {
            return next_();
        } else {
            return on_failure();
        }
    }

    timeout_ -= delta;
    if (timeout_ <= 0) {
        return on_failure();
    }

    return null_scene();
}



ScenePtr MultiplayerCoOpAwaitChrLockScene::update(Time delta)
{
    auto on_failure = [&] {
        if (auto chr = Character::find_by_id(id_).first) {
            chr->co_op_release_lock();
        }
        PLATFORM.speaker().play_sound("beep_error", 2);
        return make_scene<ReadyScene>();
    };

    if (auto scene = ActiveWorldScene::update(delta)) {
        on_failure();
        return scene;
    }

    if (result_) {
        if (*result_) {
            return next_();
        } else {
            return on_failure();
        }
    }

    timeout_ -= delta;
    if (timeout_ <= 0) {
        return on_failure();
    }

    return null_scene();
}



} // namespace skyland
