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



class Crane : public Room
{
public:
    Crane(Island* parent, const RoomCoord& position, const char* n = name());


    void update(Platform&, App&, Microseconds delta) override;
    void display(Platform::Screen& screen) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;



    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;

    void render_scaffolding(App& app, TileId buffer[16][16]) override
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::skyland_forever_unsupported |
               RoomProperties::multiplayer_unsupported;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {3, 2};
    }


    static const char* name()
    {
        return "crane";
    }


    static SystemString ui_name()
    {
        return SystemString::block_crane;
    }


    static Icon icon()
    {
        return 520;
    }


    static Icon unsel_icon()
    {
        return 504;
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    void retract()
    {
        state_ = State::retract;
    }


    void apply_damage(Platform& pfrm, App& app, Health damage) override
    {
        if (state_ == State::idle) {
            Room::apply_damage(pfrm, app, damage);
        } else {
            // Does not take damage while dropping/retracting.
        }
    }


private:
    Microseconds timer_;

    enum class State {
        idle,
        drop,
        retract,
    } state_ = State::idle;
};



} // namespace skyland