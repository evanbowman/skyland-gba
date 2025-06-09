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


#include "number/numeric.hpp"
#include "platform/key.hpp"
#include "skyland/coord.hpp"
#include <optional>
#include <tuple>



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


    virtual void update(Time delta)
    {
    }


    virtual void on_level_start()
    {
    }


    virtual void on_layout_changed(Island& island)
    {
    }


    virtual void on_room_damaged(Room& room)
    {
    }


    virtual void on_room_destroyed(Room& room)
    {
    }


    virtual void on_room_plundered(Room& room)
    {
    }


    virtual bool key_up(Key k)
    {
        return false;
    }


    virtual void update_ai(int arg)
    {
    }


    virtual bool key_down(Key k)
    {
        return false;
    }


    virtual bool key_pressed(Key k)
    {
        return false;
    }


    // key_held and key_held_reset should implement some sort of timer-based key
    // states.
    virtual bool key_held(Key k, Time duration)
    {
        return false;
    }


    virtual void key_held_reset(Key k, Time decrement)
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

        // Keys to include. Must be terminated by
        // Key::null.
        const Key* include_list = default_key_distribute_include_list)
    {
    }


    bool test_key(Key k, Time held_time, Time held_decrement)
    {
        if (key_down(k) or key_held(k, held_time)) {
            key_held_reset(k, held_decrement);
            return true;
        }
        return false;
    }


    virtual Optional<std::tuple<Vec2<u32>, Time>> touch_released()
    {
        return {};
    }


    // Only valid if touch_held()
    virtual Vec2<Float> touch_velocity()
    {
        return {};
    }


    Optional<Vec2<u32>> tap_released()
    {
        auto info = touch_released();
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


    virtual Optional<Vec2<u32>> touch_current()
    {
        return {};
    }


    virtual bool touch_held(Time duration)
    {
        return false;
    }


    virtual void
    network_sync_cursor(const RoomCoord& cursor, u8 cursor_icon, bool near)
    {
    }


    virtual void update_weapon_targets(Time delta)
    {
    }


    virtual void reassign_all_weapon_targets()
    {
    }


    virtual void delay_autofire(Time duration)
    {
    }


    virtual void delay_crew_automation(Time duration)
    {
    }


    int rooms_lost_ = 0;
    int rooms_built_ = 0;
};



Player& player();



} // namespace skyland
