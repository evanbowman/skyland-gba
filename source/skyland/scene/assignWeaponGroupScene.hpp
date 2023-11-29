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


#include "skyland/room.hpp"
#include "worldScene.hpp"



namespace skyland
{



class AssignWeaponGroupScene : public ActiveWorldScene
{
public:
    ScenePtr<Scene> update(Microseconds delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    void display() override;


private:
    enum class State {
        select_group,
        assign_rooms,
    } state_ = State::assign_rooms;

    Room::Group current_group_ = Room::Group::one;

    u8 group_cursor_;

    std::optional<Text> msg_;
};



} // namespace skyland
