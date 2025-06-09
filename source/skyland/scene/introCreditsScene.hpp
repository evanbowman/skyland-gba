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


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class IntroCreditsScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    static void show_sunflowers(int scroll, Float darken);


    bool flower_effect_ = false;


private:
    Optional<Text> text_;
    Optional<Text> copyright_text_;
    Time timer_ = 0;
    Time bell_chime_timer_ = 0;

    Time bird_anim_timer_ = 0;
    Time bird_seq_timer_ = 0;
    Time bird_seq_timer2_ = 0;
    Time bird_seq_timer3_ = 0;
    u8 bird_anim_ = 1;
    bool skip_ = false;
    bool wait_ = true;
    u8 bell_chime_cnt_ = 0;
};



} // namespace skyland
