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



class ReadyScene final : public ActiveWorldScene
{
public:
    ScenePtr update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    bool displays_minimap() override;


private:
    Time cursor_anim_timer_ = 0;
    Time describe_room_timer_ = seconds(1);
    Optional<Text> room_description_;
    BlockChecksum island_checksums_;
    u8 cursor_anim_frame_ = 0;
    bool await_start_key_ = false;
    bool await_b_key_ = false;
};



} // namespace skyland
