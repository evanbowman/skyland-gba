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
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



extern SharedVariable transporter_reload_ms;



class Transporter : public Room
{
public:
    Transporter(Island* parent, const RoomCoord& position);


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    void update(Platform&, App&, Microseconds delta) override;
    void rewind(Platform&, App&, Microseconds delta) override;

    void ___rewind___finished_reload(Platform&, App&) override;

    void ___rewind___ability_used(Platform&, App&) override;



    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    bool non_owner_selectable() const override
    {
        return true;
    }


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const RoomCoord& cursor) override;


    void transport_occupant(Platform& pfrm,
                            App& app,
                            // NOTE: if you do not pass a destination, the
                            // transporter logic will select a random one.
                            std::optional<RoomCoord> destination = {});


    void recover_character(Platform&, App& app, const RoomCoord& pos);


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "transporter";
    }


    static SystemString ui_name()
    {
        return SystemString::block_transporter;
    }


    static Float atp_value()
    {
        return 900.f;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable;
    }


    static Icon icon()
    {
        return 904;
    }


    static Icon unsel_icon()
    {
        return 888;
    }


    bool ready() const;


    Microseconds reload_time_remaining() const override
    {
        return recharge_;
    }


    void begin_recharge()
    {
        recharge_ = 1000 * transporter_reload_ms;
    }


private:
    Microseconds recharge_ = 1000 * transporter_reload_ms;
};



void transport_character_impl(App& app,
                              Island* src_island,
                              Island* dst_island,
                              CharacterId chr_id,
                              const RoomCoord& dst);



} // namespace skyland
