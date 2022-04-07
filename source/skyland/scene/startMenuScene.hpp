////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "allocator.hpp"
#include "worldScene.hpp"



namespace skyland
{



class StartMenuScene : public WorldScene
{
public:
    StartMenuScene(int fade_direction);


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


    static constexpr const u32 option_count = 7;


    enum TransitionMode {
        kill_menu,
        cut,
        fade_sweep,
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

    void add_option(Platform&,
                    const char* str,
                    DeferredScene on_click,
                    TransitionMode transition_mode);


    DynamicMemory<Data> data_;

    int fade_direction_ = 0;

    Microseconds timer_ = 0;
};



} // namespace skyland
