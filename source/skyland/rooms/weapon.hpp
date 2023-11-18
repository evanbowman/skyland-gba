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

#include "skyland/bulkTimer.hpp"
#include "skyland/room.hpp"



namespace skyland
{



class Weapon : public Room, public Timer
{
public:
    Weapon(Island* parent,
           const char* name,
           const RoomCoord& position,
           Microseconds reload_time);


    ~Weapon();


    void timer_expired(App&) override;


    Microseconds reload_interval() const override
    {
        return reload();
    }


    virtual Microseconds reload() const = 0;


    virtual void fire(App& app) = 0;


    void override_reload_timer(Microseconds new_time) override
    {
        Timer::__override_clock(new_time);
    }


    Microseconds reload_time_remaining() const override
    {
        return Timer::remaining();
    }


    static Category category()
    {
        return Category::weapon;
    }


    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::none;
    }


    void update(App& app, Microseconds delta) override;


    void rewind(App& app, Microseconds delta) override;


    void ___rewind___finished_reload(App&) override;

    void ___rewind___ability_used(App&) override;


    bool target_pinned() const override;


    void set_target(App& app, const RoomCoord& target, bool pinned) override;


    void unset_target(App& app) override;


    std::optional<RoomCoord> get_target() const override
    {
        return target_;
    }


    int debris_tile() override
    {
        return 1;
    }


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override;


    ScenePtr<Scene> select(App& app, const RoomCoord& cursor) override;


protected:
    std::optional<RoomCoord> target_;
    bool target_pinned_ = false;
};



} // namespace skyland
