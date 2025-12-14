////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
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



void make_construction_effect(Vec2<Fixnum> pos);



class Bell final : public Decoration
{
public:
    Bell(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::bell;
    }


    void render_exterior(App* app, TileId buffer[16][16]) override
    {
        render_interior(app, buffer);
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_bell)->c_str();
    }


    static const constexpr char* name()
    {
        return "bell";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bell;
    }



    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::locked_by_default | RoomProperties::fragile |
               RoomProperties::multiplayer_unsupported;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 4280;
    }


    static Icon unsel_icon()
    {
        return 4264;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void ring();


    void schedule_chimes(Time chime_spacing,
                         u8 chime_count,
                         u8 chime_repeat,
                         Time start_delay);


    void register_select_menu_options(SelectMenuScene&) override;


private:
    Time timer_ = 0;
    Time repeat_timer_ = 0;

    Time chime_spacing_ = 0;
    u8 chime_count_ = 0;

    bool repeat_on_ = false;
    u8 reps_completed_ = 0;
    u8 chime_repeat_ = 0;
};



} // namespace skyland
