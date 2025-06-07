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

#include "decoration.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Switch : public Decoration
{
public:
    Switch(Island* parent, const RoomCoord& position);


    static const char* name()
    {
        return "switch";
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    ScenePtr select(const RoomCoord& cursor) override;


    virtual lisp::Value* serialize() override;
    virtual void deserialize(lisp::Value*) override;


    static void format_description(StringBuffer<512>& buffer);


    bool description_visible() override
    {
        return true;
    }


    static SystemString ui_name()
    {
        return SystemString::block_switch;
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile;
    }


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;

    RoomCoord branch_1_;
    RoomCoord branch_2_;

    bool setup_ = false;

private:
    bool on_ = true;
};



} // namespace skyland
