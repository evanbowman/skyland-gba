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


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    bool displays_minimap() override;


private:
    DynamicMemory<bool[16][16]> matrix_;
    Time cursor_anim_timer_ = 0;
    BlockChecksum island_checksums_;
    CharacterId chr_id_;
    u8 cursor_anim_frame_ = 0;
    const bool near_;
};



} // namespace skyland
