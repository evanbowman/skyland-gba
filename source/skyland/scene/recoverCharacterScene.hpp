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


#include "skyland/coord.hpp"
#include "worldScene.hpp"



namespace skyland
{



class Room;



class RecoverCharacterScene : public ActiveWorldScene
{
public:
    RecoverCharacterScene(const RoomCoord& transporter_loc);


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


    void enter(App&, Scene& prev) override;


    void exit(App&, Scene& prev) override;


private:
    RoomCoord transporter_loc_;
    std::optional<Text> text_;
};



} // namespace skyland
