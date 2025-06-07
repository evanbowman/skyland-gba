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
                Time flight_timer,
                u8 color,
                bool near,
                bool flip);


    void update(Time delta) override;


    void rewind(Time delta) override;


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
    void roost(Island* island, Time delta);

    Platform::DynamicTexturePtr dt_;
    RoomCoord position_;
    bool near_;
    bool alerted_ = false;

    Time flight_timer_ = 0;

    enum class State {
        roost,
        fly,
        caw,
    } state_ = State::roost;

    Time anim_timer_ = 0;

    u8 anim_index_ = 0;
    u8 color_ = 0;
    Float speed_ = 0;
};



} // namespace skyland
