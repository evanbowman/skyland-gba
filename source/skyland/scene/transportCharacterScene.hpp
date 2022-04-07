////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "notificationScene.hpp"



namespace skyland
{



class TransportCharacterScene : public NotificationScene
{
public:
    TransportCharacterScene(Platform& pfrm, Vec2<u8> origin);


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    std::optional<DynamicMemory<bool[16][16]>> matrix_;

    Microseconds cursor_anim_timer_ = 0;
    u8 cursor_anim_frame_ = 0;

    Vec2<u8> origin_;
};



} // namespace skyland
