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



namespace skyland
{

class Detail : public Room
{
public:
    Detail(Island* parent, const RoomCoord& position, u16 tile) : Room(parent)
    {
    }


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;
};
} // namespace skyland
