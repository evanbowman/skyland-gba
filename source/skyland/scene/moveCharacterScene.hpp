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
#include "skyland/characterId.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ModifyCharacterScene : public ActiveWorldScene
{
public:
    ModifyCharacterScene(CharacterId chr_id, bool near);


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    bool modify_name_ = false;


private:
    DynamicMemory<bool[16][16]> matrix_;
    Microseconds cursor_anim_timer_ = 0;
    Microseconds chr_name_timer_ = 0;
    u8 cursor_anim_frame_ = 0;
    CharacterId chr_id_;
    const bool near_;
};



} // namespace skyland
