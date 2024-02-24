////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> update(Time delta) override;


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
