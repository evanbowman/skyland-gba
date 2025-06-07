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



class HintScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;

    ScenePtr update(Time delta) override;

private:
    u32 hint_index_ = 0;

    enum class State {
        scene_intro,
        // scene_exit,
        fade_out,
        swap_img,
        fade_in,
        idle,
    } state_ = State::scene_intro;

    Time timer_ = 0;

    Optional<Text> heading_;
    Optional<TextView> body_;

    Buffer<u8, 24> index_sequence_;
};



} // namespace skyland
