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


    void update(App& app, Microseconds delta) override;


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


    ScenePtr<Scene> select(App& app, const RoomCoord& cursor) override;


    void set_message(const char* msg)
    {
        *data_ = msg;
    }


    bool opponent_display_on_hover() const override;


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const RoomCoord& cursor) override;


private:
    DynamicMemory<StringBuffer<400>> data_;
    std::optional<Platform::DynamicTexturePtr> hint_img_;
    Microseconds hint_img_release_timer_ = 0;
};



} // namespace skyland
