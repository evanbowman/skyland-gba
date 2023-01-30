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
#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class StartMenuScene : public Scene
{
public:
    StartMenuScene(int fade_direction, int default_cursor = 0);


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


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

    void add_option(Platform&,
                    const char* str,
                    DeferredScene on_click,
                    TransitionMode transition_mode);


    DynamicMemory<Data> data_;

    int fade_direction_ = 0;

    Microseconds timer_ = 0;
    int start_y_ = 3;

    bool preserve_transparency_ = false;
};



} // namespace skyland
