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



class RewindScene : public Scene
{
public:
    RewindScene(bool is_far_camera) : far_camera_(is_far_camera)
    {
    }

    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    void display(Platform&, App&) override;


private:
    void print_timestamp(Platform& pfrm, App& app);


    std::optional<Text> text_;
    std::optional<Text> speed_text1_;
    std::optional<Text> speed_text2_;
    std::optional<Text> speed_text3_;
    int speed_ = 0;
    bool far_camera_;
};



} // namespace skyland