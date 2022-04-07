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

#include "bird.hpp"
#include "platform/platform.hpp"



namespace skyland
{



class GenericBird : public Bird
{
public:
    GenericBird(Platform::DynamicTexturePtr dt,
                const Vec2<u8>& coord,
                bool near = false);


    // Alternate constructor, intended for rewind implementation.
    GenericBird(Platform::DynamicTexturePtr dt,
                const Vec2<u8>& coord,
                const Vec2<Float>& position,
                Float speed,
                Microseconds flight_timer,
                u8 color,
                bool near,
                bool flip);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void signal(Platform&, App&) override;



    Island* island(App& app) override;


    static void spawn(Platform& pfrm, App& app, Island& island, int count);


    static void generate(Platform& pfrm, App& app);


    Vec2<u8> coordinate() override
    {
        return position_;
    }


private:
    void roost(Platform& pfrm, Island* island, Microseconds delta);

    Platform::DynamicTexturePtr dt_;
    Vec2<u8> position_;
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
