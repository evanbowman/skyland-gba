////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Deflector : public Room
{
public:
    Deflector(Island* parent, const RoomCoord& position);


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::sylph_only |
               RoomProperties::manufactory_required |
               RoomProperties::accepts_ion_damage;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 900.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "deflector";
    }


    static SystemString ui_name()
    {
        return SystemString::block_deflector;
    }


    static Icon icon()
    {
        return 2696;
    }


    static Icon unsel_icon()
    {
        return 2712;
    }


    bool allows_powerdown() override;


    void on_powerchange() override;


    void project_deflector_shield() override;


private:
    Time active_timer_ = 0;
};



} // namespace skyland
