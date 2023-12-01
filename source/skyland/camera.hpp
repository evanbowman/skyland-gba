////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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


    virtual void update(Island& target,
                        const RoomCoord& cursor_loc,
                        Microseconds delta,
                        bool near);



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

    Microseconds shake_timer_ = 0;
    int shake_magnitude_ = 0;
};



} // namespace skyland
