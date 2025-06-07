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


class PluginRoom : public Room
{
public:
    PluginRoom(Island* parent, const RoomCoord& position, RoomMeta* metaclass);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void set_target(const RoomCoord& target, bool pinned) override;


    void unset_target() override;


    ScenePtr select_impl(const RoomCoord& cursor) override;


    bool description_visible() override
    {
        return true;
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


private:
    Time timer_ = 0;
    Optional<RoomCoord> target_;
};



} // namespace skyland
