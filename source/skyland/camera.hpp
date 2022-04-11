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

#include "coord.hpp"
#include "number/numeric.hpp"



class Platform;



namespace skyland
{



class App;
class Island;



class Camera
{
public:
    virtual ~Camera()
    {
    }


    virtual void update(Platform& pfrm,
                        App& app,
                        Island& target,
                        const RoomCoord& cursor_loc,
                        Microseconds delta,
                        bool near);



    void shake(int magnitude = 12);


    bool is_shaking() const
    {
        return shake_magnitude_;
    }


    void reset()
    {
        target_ = {};
        current_ = {};
    }


    virtual bool always_update(Platform& pfrm)
    {
        return false;
    }


    virtual void reset_default(App& app)
    {
    }


    Vec2<Float> center() const
    {
        return current_;
    }


protected:
    Vec2<int> target_;
    Vec2<Float> current_;

    Microseconds shake_timer_ = 0;
    int shake_magnitude_ = 0;
};



} // namespace skyland
