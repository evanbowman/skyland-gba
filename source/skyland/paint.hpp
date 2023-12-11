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

#include "skyland/scene/module.hpp"



// Implements a simplistic paint program.



namespace skyland
{



class Paint
{
public:
    Paint(u8 canvas_width = 13,
          u8 canvas_height = 11,
          u8 canvas_origin_x = 3,
          u8 canvas_origin_y = 3,
          int view_shift = -18)
        : view_shift_(view_shift), width_(canvas_width), height_(canvas_height),
          origin_x_(canvas_origin_x), origin_y_(canvas_origin_y)
    {
    }


    virtual ~Paint()
    {
    }


    ScenePtr<Scene> update(Time delta);


    void display();


    void init();


    virtual void show();
    void draw_rulers();


    virtual u8 get_pixel(u8 x, u8 y) = 0;
    virtual void set_pixel(u8 x, u8 y, u8 value) = 0;


    u8 width() const
    {
        return width_;
    }


    u8 height() const
    {
        return height_;
    }


protected:
    Time cursor_move_tic_ = 0;

    bool ready_ = false;

    u16 palette_[16];


    Vec2<u8> cursor_;
    u32 color_ = 0;
    int view_shift_;

    u8 width_;
    u8 height_;
    u8 origin_x_;
    u8 origin_y_;
};



} // namespace skyland
