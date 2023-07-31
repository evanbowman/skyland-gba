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


    void enter(Platform& pfrm, App& app, Scene& scene) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void display(Platform& pfrm, App& app) override;


    Island* island(App& app) const;


private:
    struct Options
    {
        static constexpr int cap = 10;

        Buffer<Text, cap> lines_;
        Buffer<SystemString, cap> strings_;
        Buffer<Function<16, ScenePtr<Scene>(Platform&, App&)>, cap> callbacks_;
        u8 longest_line_;

        Bitvector<cap> specific_;

        Buffer<SystemString, cap> pushed_strings_;
    };

    void redraw_line(Platform& pfrm, int line, bool highlight);

    DynamicMemory<Options> opts_;
    int sel_ = 0;
};



} // namespace skyland
