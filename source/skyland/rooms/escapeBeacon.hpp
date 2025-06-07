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
#include "skyland/systemString.hpp"



namespace skyland
{



class EscapeBeacon final : public Room
{
public:
    EscapeBeacon(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static const char* name()
    {
        return "escape-beacon";
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1000.0_atp;
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
        return RoomProperties::roof_hidden |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible |
               RoomProperties::skyland_forever_unsupported;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    Time reload_time_remaining() const override
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
    Time timer_;
};



} // namespace skyland
