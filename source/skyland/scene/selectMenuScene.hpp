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
#include "skyland/island.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectMenuScene : public ActiveWorldScene
{
public:
    SelectMenuScene() : opts_(allocate_dynamic<Options>("sel-opts"))
    {
    }


    void enter(Scene& scene) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    Island* island() const;


private:
    struct Options
    {
        static constexpr int cap = 10;

        Buffer<Text, cap> lines_;
        Buffer<SystemString, cap> strings_;
        Buffer<StringBuffer<8>, cap> suffixes_;
        Buffer<Function<16, ScenePtr()>, cap> callbacks_;
        u8 longest_line_;

        Bitvector<cap> specific_;

        Buffer<SystemString, cap> pushed_strings_;
    };

    void redraw_line(int line, bool highlight);

    DynamicMemory<Options> opts_;
    int sel_ = 0;
    bool show_power_on_exit_ = false;
};



} // namespace skyland
