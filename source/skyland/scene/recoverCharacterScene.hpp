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


#include "skyland/coord.hpp"
#include "worldScene.hpp"



namespace skyland
{



class Room;



class RecoverCharacterScene : public ActiveWorldScene
{
public:
    RecoverCharacterScene(const RoomCoord& transporter_loc);


    ScenePtr update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


private:
    RoomCoord transporter_loc_;
    Optional<Text> text_;
};



} // namespace skyland
