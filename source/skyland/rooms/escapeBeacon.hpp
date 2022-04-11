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
#include "skyland/systemString.hpp"



namespace skyland
{



class EscapeBeacon : public Room
{
public:
    EscapeBeacon(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    static const char* name()
    {
        return "escape-beacon";
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1000.f;
    }


    static Vec2<u8> size()
    {
        return {1, 3};
    }


    static SystemString ui_name()
    {
        return SystemString::block_escape_beacon;
    }


    static Icon icon()
    {
        return 2216;
    }


    static Icon unsel_icon()
    {
        return 2232;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::skyland_forever_unsupported;
    }


    ScenePtr<Scene> select(Platform&, App&, const Vec2<u8>& cursor) override;


    Microseconds reload_time_remaining() const override
    {
        if (not activated_) {
            return 0;
        } else {
            return timer_;
        }
    }


    void reset_state() override
    {
        activated_ = false;
        timer_ = 0;
    }


private:
    bool activated_ = false;
    Microseconds timer_;
};



} // namespace skyland
