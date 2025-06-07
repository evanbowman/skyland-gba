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



class CargoBay final : public Room
{
public:
    CargoBay(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void display(Platform::Screen&) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;

    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static ATP atp_value()
    {
        return 800.0_atp;
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
