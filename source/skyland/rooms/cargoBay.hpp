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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class CargoBay final : public Room
{
public:
    CargoBay(Island* parent, const RoomCoord& position);


    void update(Microseconds delta) override;


    void display(Platform::Screen&) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static Float atp_value()
    {
        return 800.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "cargo-bay";
    }


    static SystemString ui_name()
    {
        return SystemString::block_cargo_bay;
    }


    static Icon icon()
    {
        return 1320;
    }


    static Icon unsel_icon()
    {
        return 1336;
    }


    const char* cargo() const
    {
        return cargo_;
    }


    u8 cargo_count() const
    {
        return count_;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::not_constructible;
    }


    virtual lisp::Value* serialize() override;
    virtual void deserialize(lisp::Value*) override;


    bool set_cargo(const char* cargo, u8 count);


    void finalize() override;


private:
    char cargo_[19];
    u8 count_;
};



} // namespace skyland
