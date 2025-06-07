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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Cloak : public Room
{
public:
    Cloak(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Time delta) override;
    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    void render_cloak(TileId buffer[16][16]) override;


    bool cloaks_coordinate(const RoomCoord& c) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::accepts_ion_damage | RoomProperties::sylph_only |
               RoomProperties::manufactory_required;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 500.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "cloak";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cloak;
    }


    static Icon icon()
    {
        return 3832;
    }


    static Icon unsel_icon()
    {
        return 3816;
    }

private:
    int timer_ = 0;
};



} // namespace skyland
