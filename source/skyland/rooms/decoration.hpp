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

#include "skyland/room.hpp"
#include "skyland/tileId.hpp"



namespace skyland
{



class Decoration : public Room
{
public:
    using Room::Room;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1.0_atp;
    }


    static Category category()
    {
        return Category::decoration;
    }
};



} // namespace skyland
