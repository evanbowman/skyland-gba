////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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


    ScenePtr update(Time delta) override;


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


    ScenePtr update(Time delta) override;


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
