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

#include "bird.hpp"
#include "platform/platform.hpp"



namespace skyland
{



class GenericBird : public Bird
{
public:
    GenericBird(Platform::DynamicTexturePtr dt,
                const RoomCoord& coord,
                bool near = false);


    // Alternate constructor, intended for rewind implementation.
    GenericBird(Platform::DynamicTexturePtr dt,
                const RoomCoord& coord,
                const Vec2<Fixnum>& position,
                Float speed,
                Microseconds flight_timer,
                u8 color,
                bool near,
                bool flip);


    void update(Microseconds delta) override;


    void rewind(Microseconds delta) override;


    void signal() override;



    Island* island() override;


    static void spawn(Island& island, int count);


    static void generate();


    RoomCoord coordinate() override
    {
        return position_;
    }


    u8 icon() const override
    {
        return (color_) ? 19 : 20;
    }


private:
    void roost(Island* island, Microseconds delta);

    Platform::DynamicTexturePtr dt_;
    RoomCoord position_;
    bool near_;
    bool alerted_ = false;

    Microseconds flight_timer_ = 0;

    enum class State {
        roost,
        fly,
        caw,
    } state_ = State::roost;

    Microseconds anim_timer_ = 0;

    u8 anim_index_ = 0;
    u8 color_ = 0;
    Float speed_ = 0;
};



} // namespace skyland
