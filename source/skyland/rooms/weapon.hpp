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

#include "skyland/room.hpp"


namespace skyland
{



class Weapon : public Room
{
public:
    Weapon(Island* parent,
           const char* name,
           const RoomCoord& position,
           Microseconds reload_time);


    virtual Microseconds reload() const = 0;


    virtual void fire(Platform& pfrm, App& app) = 0;


    Microseconds reload_time_remaining() const override
    {
        return reload_timer_;
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


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void ___rewind___finished_reload(Platform&, App&) override;

    void ___rewind___ability_used(Platform&, App&) override;


    void set_target(Platform& pfrm, App& app, const RoomCoord& target) override;


    void unset_target(Platform& pfrm, App& app) override;


    std::optional<RoomCoord> get_target() const override
    {
        return target_;
    }


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override;


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


protected:
    std::optional<RoomCoord> target_;
    Microseconds reload_timer_;
};



} // namespace skyland
