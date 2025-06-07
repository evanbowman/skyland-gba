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



#include "skyland/blockChecksum.hpp"
#include "worldScene.hpp"



namespace skyland
{



class InspectP2Scene final : public ActiveWorldScene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void display() override;


    bool displays_minimap() override;


public:
    Time cursor_anim_timer_;
    Time describe_room_timer_ = seconds(1);
    Optional<Text> room_description_;
    BlockChecksum island_checksums_;
    u8 cursor_anim_frame_;
    bool await_start_key_ = false;
    bool await_b_key_ = false;
};



} // namespace skyland
