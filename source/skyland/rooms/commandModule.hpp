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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class CommandModule final : public Room
{
public:
    CommandModule(Island* parent,
                  const RoomCoord& position,
                  const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::singleton;;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float atp_value()
    {
        return 20.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "command-module";
    }


    static SystemString ui_name()
    {
        return SystemString::block_command_module;
    }


    static Icon icon()
    {
        return 3992;
    }


    static Icon unsel_icon()
    {
        return 3976;
    }


private:
    Microseconds next_action_timer_ = seconds(1);

    using IdBuffer = Buffer<CharacterId, 80>;

    struct IdBuffers
    {
        IdBuffer local_;
        IdBuffer boarded_;
    };

    DynamicMemory<IdBuffers> id_buffers_;
    u32 local_buffer_index_ = 0;
    u32 boarded_buffer_index_ = 0;

    u16 room_check_index_ = 0;
};



} // namespace skyland
