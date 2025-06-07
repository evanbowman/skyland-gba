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
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class SelectSampleScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


private:
    void show_options();

    enum class State {
        fade_in,
        idle,
        fade_out,
    } state_ = State::fade_in;


    Optional<lisp::Protected> samples_;
    Buffer<Text, 5> text_;

    u8 page_ = 0;
    u8 cursor_ = 0;

    int page_count_ = 0;

    Time timer_ = 0;

    bool exit_ = false;
};



} // namespace skyland
