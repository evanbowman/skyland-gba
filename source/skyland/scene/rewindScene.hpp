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



class RewindScene : public Scene
{
public:
    RewindScene(bool is_far_camera) : far_camera_(is_far_camera)
    {
    }

    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    void display() override;


private:
    void print_timestamp();


    Optional<Text> text_;
    Optional<Text> speed_text1_;
    Optional<Text> speed_text2_;
    Optional<Text> speed_text3_;
    int speed_ = 0;
    bool far_camera_;
    bool exit_requested_ = false;
};



} // namespace skyland
