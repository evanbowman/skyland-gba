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


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class IntroCreditsScene : public Scene
{
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static void show_sunflowers(Platform& pfrm, int scroll, Float darken);


    bool flower_effect_ = false;


private:
    bool wait_ = true;
    bool exit_ = false;
    std::optional<Text> text_;
    std::optional<Text> copyright_text_;
    Microseconds timer_ = 0;
    Microseconds flower_effect_timer_ = 0;

    Microseconds bird_anim_timer_ = 0;
    Microseconds bird_seq_timer_ = 0;
    Microseconds bird_seq_timer2_ = 0;
    Microseconds bird_seq_timer3_ = 0;
    u8 bird_anim_ = 1;
    u8 bird_state_ = 0;
};



} // namespace skyland
