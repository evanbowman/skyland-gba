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


#include "skyland/room.hpp"
#include "worldScene.hpp"



namespace skyland
{



class AssignWeaponGroupScene : public ActiveWorldScene
{
public:
    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    void display() override;


private:
    enum class State {
        select_group,
        assign_rooms,
    } state_ = State::assign_rooms;

    Optional<Text> msg_;
};



} // namespace skyland
