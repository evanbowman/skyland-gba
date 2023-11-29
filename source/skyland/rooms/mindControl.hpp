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



class MindControl final : public Room
{
public:
    MindControl(Island* parent,
                const RoomCoord& position,
                const char* n = name());


    void update(Microseconds delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden |
               RoomProperties::manufactory_required |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float atp_value()
    {
        return 900.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "mind-control";
    }


    static SystemString ui_name()
    {
        return SystemString::block_mind_control;
    }


    static Icon icon()
    {
        return 4040;
    }


    static Icon unsel_icon()
    {
        return 4024;
    }


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override;


    void bind_character(CharacterId id)
    {
        controlled_character_ = id;
    }


    CharacterId bound_character() const
    {
        return controlled_character_;
    }


    void finalize() override;


    void unset_target() override
    {
        controlled_character_ = 0;
    }


private:
    Microseconds next_action_timer_ = seconds(1);

    CharacterId controlled_character_ = 0;
};



} // namespace skyland
