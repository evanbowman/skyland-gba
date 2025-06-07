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
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class StartMenuScene : public Scene
{
public:
    StartMenuScene(int fade_direction, int default_cursor = 0);


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    bool cascade_anim_in_ = false;

private:
    enum class State {
        init,
        enter,
        idle,
        clear,
        partial_clear,
        fade_out,
        sweep_up,
        after_sweep,
        exit,
        cut,
    } state_ = State::init;


    static constexpr const u32 option_count = 10;


    enum TransitionMode {
        kill_menu,
        cut,
        fade_sweep,
        fade_sweep_transparent_text,
    };


    struct Data
    {
        Buffer<Text, option_count> text_;

        Buffer<StringBuffer<32>, option_count> disp_queue_;
        u8 disp_count_;
        Time disp_timer_ = 0;

        struct TransitionInfo
        {
            DeferredScene next_scene_;
            TransitionMode mode_;
        };
        Buffer<TransitionInfo, option_count> on_click_;
        Buffer<StringBuffer<32>, option_count> option_names_;

        u8 cursor_ = 0;
    };

    Float y_offset_ = 0.f;

    Float diff_percent_ = 0.3f;
    int add_offset_ = 0;

    void add_option(const char* str,
                    DeferredScene on_click,
                    TransitionMode transition_mode);


    DynamicMemory<Data> data_;

    int fade_direction_ = 0;

    Time timer_ = 0;
    int start_y_ = 3;

    bool preserve_transparency_ = false;
};



} // namespace skyland
