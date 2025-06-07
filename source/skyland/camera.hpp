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


    virtual void
    update(Island& target, const RoomCoord& cursor_loc, Time delta, bool near);



    void shake(int magnitude = 12);


    bool is_shaking() const
    {
        return shake_magnitude();
    }


    u8 shake_magnitude() const
    {
        return shake_magnitude_;
    }


    void reset()
    {
        target_ = {};
        current_ = {};
    }


    virtual bool always_update()
    {
        return false;
    }


    virtual void reset_default()
    {
    }


    Vec2<Float> center() const
    {
        return current_;
    }


protected:
    Vec2<int> target_;
    Vec2<Float> current_;

    Time shake_timer_ = 0;
    int shake_magnitude_ = 0;
};



} // namespace skyland
