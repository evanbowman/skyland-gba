////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "decoration.hpp"
#include "skyland/img.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"
#include "skyland/tileId.hpp"



namespace skyland
{



class Canvas : public Decoration
{
public:
    Canvas(Island* parent, const RoomCoord& position);


    ~Canvas();


    static int default_health()
    {
        return 1;
    }


    static int default_cost()
    {
        return 1;
    }


    static int default_power()
    {
        return 0;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::only_constructible_in_sandbox |
               RoomProperties::fragile;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static SystemString ui_name()
    {
        return SystemString::block_generic;
    }


    static const char* name()
    {
        return "canvas";
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;


    ScenePtr select_impl(const RoomCoord& cursor) override;


    lisp::Value* serialize() override;


    void deserialize(lisp::Value* v) override;


    void bind_graphics(const img::Image& img);


    void finalize() override;


    void on_salvage() override;


    void record_removed();


    static Icon icon()
    {
        return 2664;
    }


    static Icon unsel_icon()
    {
        return 2680;
    }


    static void format_description(StringBuffer<512>& buffer);


    void publish_tiles();


    using ImagePtr = UniquePtr<img::Image, void (*)(img::Image*)>;


private:
    TileId tile_;
    int canvas_texture_slot_ = -1;

    Optional<ImagePtr> img_data_;
};



} // namespace skyland
