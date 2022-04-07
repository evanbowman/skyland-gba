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


    ScenePtr<Scene> update(Platform& pfrm, App& app, Microseconds delta);


    void display(Platform& pfrm, App& app);


    void init(Platform&, App&);


    virtual void show(Platform&, App&);
    void draw_rulers(Platform&);


    virtual u8 get_pixel(App& app, u8 x, u8 y) = 0;
    virtual void set_pixel(App& app, u8 x, u8 y, u8 value) = 0;


    u8 width() const
    {
        return width_;
    }


    u8 height() const
    {
        return height_;
    }


private:
    Microseconds cursor_move_tic_ = 0;

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
