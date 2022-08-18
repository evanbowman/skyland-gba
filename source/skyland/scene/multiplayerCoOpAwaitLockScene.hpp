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


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


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
    std::optional<bool> result_;

    Microseconds timeout_ = milliseconds(500);
};



class MultiplayerCoOpAwaitChrLockScene : public ActiveWorldScene
{
public:
    MultiplayerCoOpAwaitChrLockScene(DeferredScene next, CharacterId id)
        : next_(next), id_(id)
    {
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


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
    std::optional<bool> result_;

    Microseconds timeout_ = milliseconds(500);
};



} // namespace skyland
