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


#include "player.hpp"



namespace skyland
{



// An implementation of Player, controlled directly by a player via the device's
// physical buttons.



class PlayerP1 : public Player
{
public:
    void update(Platform&, App&, Microseconds delta) override;


    void on_room_destroyed(Platform& pfrm, App& app, Room& room) override;


    void on_room_damaged(Platform& pfrm, App& app, Room& room);


    void on_room_plundered(Platform& pfrm, App& app, Room& room) override;


    bool key_down(Platform&, Key k) override;


    bool key_up(Platform&, Key k) override;


    bool key_pressed(Platform&, Key k) override;


    bool key_held(Key k, Microseconds duration) override;


    void key_held_reset(Key k, Microseconds decrement) override;


    void key_held_distribute(Platform& pfrm, const Key* include_list) override;


    std::optional<std::tuple<Vec2<u32>, Microseconds>>
    touch_released(Platform& pfrm) override;


    std::optional<Vec2<u32>> touch_current(Platform& pfrm) override;


    bool touch_held(Microseconds duration) override;


    void touch_consume() override;


    Vec2<Float> touch_velocity(Platform& pfrm) override
    {
        return touch_velocity_;
    }


private:
    Microseconds last_key_ = 0;

    Microseconds touch_held_time_ = 0;
    Microseconds last_touch_held_time_ = 0;

    bool touch_invalidate_ = false;
    Vec2<u32> last_touch_;
    Vec2<Float> touch_velocity_;

    Microseconds key_held_timers_[static_cast<int>(Key::count)];
};



} // namespace skyland
