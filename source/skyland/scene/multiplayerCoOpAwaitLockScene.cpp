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


#include "multiplayerCoOpAwaitLockScene.hpp"



namespace skyland
{



ScenePtr<Scene> MultiplayerCoOpAwaitLockScene::update(Platform& pfrm,
                                                      App& app,
                                                      Microseconds delta)
{
    auto on_failure = [&] {
        if (auto room = player_island(app).get_room(coord_)) {
            // Before entering this scene, the originating code should
            // have checked the room's lock and acquired it.
            room->co_op_peer_release_lock();
        }
        pfrm.speaker().play_sound("beep_error", 2);
        return scene_pool::alloc<ReadyScene>();
    };

    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
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



ScenePtr<Scene> MultiplayerCoOpAwaitChrLockScene::update(Platform& pfrm,
                                                         App& app,
                                                         Microseconds delta)
{
    auto on_failure = [&] {
        if (auto chr = BasicCharacter::find_by_id(app, id_).first) {
            chr->co_op_release_lock();
        }
        pfrm.speaker().play_sound("beep_error", 2);
        return scene_pool::alloc<ReadyScene>();
    };

    if (auto scene = ActiveWorldScene::update(pfrm, app, delta)) {
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
