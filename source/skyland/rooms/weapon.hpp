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
           const Vec2<u8>& position,
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


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    static u32 properties()
    {
        return RoomProperties::none;
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void ___rewind___finished_reload(Platform&, App&) override;

    void ___rewind___ability_used(Platform&, App&) override;


    void set_target(Platform& pfrm, App& app, const Vec2<u8>& target) override;


    void unset_target(Platform& pfrm, App& app) override;


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const Vec2<u8>& cursor) override;


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor) override;


protected:
    std::optional<Vec2<u8>> target_;
    Microseconds reload_timer_;
};



} // namespace skyland
