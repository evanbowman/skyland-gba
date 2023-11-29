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

#include "allocator.hpp"
#include "notificationScene.hpp"
#include "skyland/coord.hpp"



namespace skyland
{



class TransportCharacterScene : public NotificationScene
{
public:
    TransportCharacterScene(RoomCoord origin);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr<Scene> update(Microseconds delta) override;


    void display() override;


private:
    std::optional<DynamicMemory<bool[16][16]>> matrix_;

    Microseconds cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;

    RoomCoord origin_;
};



} // namespace skyland
