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


#include "script/lisp.hpp"
#include "skyland/coord.hpp"
#include "skyland/room.hpp"
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


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


private:
    lisp::Protected parameters_;
    bool near_;
    bool started_near_;

    Optional<Vec2<u8>> required_space_;

    Optional<Text> text_;
    RoomCoord cached_near_cursor_;
    RoomCoord cached_far_cursor_;
    Time cursor_anim_timer_ = 0;
    Time flicker_timer_ = 0;

    bool flicker_on_ = false;

    bool cursor_anim_frame_ = false;

    Room::WeaponOrientation w_ot_ = Room::WeaponOrientation::none;
};



} // namespace skyland
