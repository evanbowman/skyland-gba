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

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class MirrorHull final : public Hull
{
public:
    MirrorHull(Island* parent, const RoomCoord& position);


    void update(Platform&, App&, Microseconds delta) override;
    void rewind(Platform&, App&, Microseconds delta) override;


    static const char* name()
    {
        return "mirror-hull";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mirror_hull;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_mirror_hull)->c_str();
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required | Hull::properties() |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::locked_by_default;
    }


    static Icon icon()
    {
        return 1960;
    }


    static Icon unsel_icon()
    {
        return 1976;
    }


    static Float atp_value()
    {
        return 0.5f;
    }


    TileId tile() const;


    void render_interior(App& app, TileId buffer[16][16]) override;


    void render_exterior(App& app, TileId buffer[16][16]) override;
};



} // namespace skyland
