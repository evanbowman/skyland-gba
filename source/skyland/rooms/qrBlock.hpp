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
#include "skyland/tile.hpp"



namespace skyland
{



class QrBlock final : public Decoration
{
public:
    QrBlock(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position),
          data_(allocate_dynamic<StringBuffer<400>>("qr-data"))
    {
    }


    void update(Time delta) override;


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_qr)->c_str();
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::not_constructible;
    }


    static const char* name()
    {
        return "code";
    }


    static SystemString ui_name()
    {
        return SystemString::block_qr;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 3800;
    }


    static Icon unsel_icon()
    {
        return 3784;
    }


    bool non_owner_selectable() const override
    {
        return true;
    }


    static int default_power()
    {
        return 0;
    }

    static int default_cost()
    {
        return 1;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void set_message(const char* msg)
    {
        *data_ = msg;
    }


    bool opponent_display_on_hover() const override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


private:
    DynamicMemory<StringBuffer<400>> data_;
    Optional<Platform::DynamicTexturePtr> hint_img_;
    Time hint_img_release_timer_ = 0;
};



} // namespace skyland
