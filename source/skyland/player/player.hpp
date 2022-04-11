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


#include "number/numeric.hpp"
#include "platform/key.hpp"
#include <tuple>
#include "skyland/coord.hpp"



class Platform;



namespace skyland
{



class Island;
class Room;
class App;



class Player
{
public:
    virtual ~Player()
    {
    }


    virtual void update(Platform&, App&, Microseconds delta)
    {
    }


    virtual void on_layout_changed(Platform& pfrm, App& app, Island& island)
    {
    }


    virtual void on_room_damaged(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual void on_room_destroyed(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual void on_room_plundered(Platform& pfrm, App& app, Room& room)
    {
    }


    virtual bool key_up(Platform&, Key k)
    {
        return false;
    }


    virtual bool key_down(Platform&, Key k)
    {
        return false;
    }


    virtual bool key_pressed(Platform&, Key k)
    {
        return false;
    }


    // key_held and key_held_reset should implement some sort of timer-based key
    // states.
    virtual bool key_held(Key k, Microseconds duration)
    {
        return false;
    }


    virtual void key_held_reset(Key k, Microseconds decrement)
    {
    }


    constexpr static const Key default_key_distribute_include_list[5] =
        {Key::left, Key::right, Key::up, Key::down, Key::null};


    // Should be implemented to treat all currently pressed keys as held for the
    // same amount of time as the longest held key. Used to implement smooth
    // scroll locking, where we want the scroll lock to continue in a different
    // direction if the player changes direction with the d-pad. E.g. press and
    // hold to scroll continuously (see test_key()), then change direction, the
    // scrolling would halt until the new direction builds up enough inertia to
    // start scrolling continuously, unless we grant the new direction the same
    // scrolling inertia as the previous direction button.
    virtual void key_held_distribute(
        Platform& pfrm,
        // Keys to include. Must be terminated by
        // Key::null.
        const Key* include_list = default_key_distribute_include_list)
    {
    }


    bool test_key(Platform& pfrm,
                  Key k,
                  Microseconds held_time,
                  Microseconds held_decrement)
    {
        if (key_down(pfrm, k) or key_held(k, held_time)) {
            key_held_reset(k, held_decrement);
            return true;
        }
        return false;
    }


    virtual std::optional<std::tuple<Vec2<u32>, Microseconds>>
    touch_released(Platform& pfrm)
    {
        return {};
    }


    // Only valid if touch_held()
    virtual Vec2<Float> touch_velocity(Platform& pfrm)
    {
        return {};
    }


    std::optional<Vec2<u32>> tap_released(Platform& pfrm)
    {
        auto info = touch_released(pfrm);
        if (info and std::get<1>(*info) < milliseconds(100)) {
            return std::get<0>(*info);
        }
        return {};
    }


    // This method should be implemented (if applicable) to detach whatever
    // touch event is currently attached to the player. Used to prevent multiple
    // codepaths from registering a touch event which already triggered some
    // other behavior.
    virtual void touch_consume()
    {
    }


    virtual std::optional<Vec2<u32>> touch_current(Platform& pfrm)
    {
        return {};
    }


    virtual bool touch_held(Microseconds duration)
    {
        return false;
    }


    virtual void network_sync_cursor(Platform& pfrm,
                                     const RoomCoord& cursor,
                                     u8 cursor_icon,
                                     bool near)
    {
    }


    int rooms_lost_ = 0;
    int rooms_built_ = 0;
};



Player& player(App& app);



} // namespace skyland
