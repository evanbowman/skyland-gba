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
        if (auto chr = BasicCharacter::find_by_id(id_).first) {
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
