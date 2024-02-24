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


#pragma once

#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/scene_pool.hpp"
#include "worldScene.hpp"



namespace skyland
{



class MultiplayerCoOpAwaitLockScene : public ActiveWorldScene
{
public:
    MultiplayerCoOpAwaitLockScene(DeferredScene next, const RoomCoord& coord)
        : next_(next), coord_(coord)
    {
    }


    ScenePtr<Scene> update(Time delta) override;


    void signal_result(bool result)
    {
        result_ = result;
    }


    MultiplayerCoOpAwaitLockScene* cast_co_op_await_lock_scene() override
    {
        return this;
    }


private:
    DeferredScene next_;
    RoomCoord coord_;
    Optional<bool> result_;

    Time timeout_ = milliseconds(500);
};



class MultiplayerCoOpAwaitChrLockScene : public ActiveWorldScene
{
public:
    MultiplayerCoOpAwaitChrLockScene(DeferredScene next, CharacterId id)
        : next_(next), id_(id)
    {
    }


    ScenePtr<Scene> update(Time delta) override;


    void signal_result(bool result)
    {
        result_ = result;
    }


    MultiplayerCoOpAwaitChrLockScene* cast_co_op_await_chr_lock_scene() override
    {
        return this;
    }


private:
    DeferredScene next_;
    CharacterId id_;
    Optional<bool> result_;

    Time timeout_ = milliseconds(500);
};



} // namespace skyland
