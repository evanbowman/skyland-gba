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


#include "script/lisp.hpp"
#include "skyland/coord.hpp"
#include "worldScene.hpp"



namespace skyland
{


// The script API provides a function through which the script writer can
// request an input coordinate from the user. This scene implements the cursor
// selection and invokes the script callback when the user makes a selection.



class SelInputScene : public ActiveWorldScene
{
public:
    SelInputScene(lisp::Value* parameters, bool near)
        : parameters_(parameters), near_(near), started_near_(near)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    lisp::Protected parameters_;
    bool near_;
    bool started_near_;

    std::optional<Vec2<u8>> required_space_;

    std::optional<Text> text_;
    RoomCoord cached_near_cursor_;
    RoomCoord cached_far_cursor_;
    Microseconds cursor_anim_timer_ = 0;

    bool cursor_anim_frame_ = false;
};



} // namespace skyland
