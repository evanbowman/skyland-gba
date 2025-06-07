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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



extern SharedVariable transporter_reload_ms;



class Transporter final : public Room
{
public:
    Transporter(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void update(Time delta) override;
    void rewind(Time delta) override;

    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;



    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    bool non_owner_selectable() const override
    {
        return true;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void transport_occupant(
        // NOTE: if you do not pass a destination, the
        // transporter logic will select a random one.
        Optional<RoomCoord> destination = {});


    void recover_character(const RoomCoord& pos);


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


    static ATP atp_value()
    {
        return 900.0_atp;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::habitable |
               RoomProperties::multiboot_compatible;
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


    Time reload_time_remaining() const override
    {
        return recharge_;
    }


    void begin_recharge();


    Time recharge_time() const;


    void finalize() override;


    void on_powerchange() override;


    bool allows_powerdown() override;


    void amplify(bool enable) override;


private:
    Time recharge_ = 1000 * transporter_reload_ms;
    bool amplify_ = false;
};



void transport_character_impl(Island* src_island,
                              Island* dst_island,
                              CharacterId chr_id,
                              const RoomCoord& dst);



void make_transport_effect(Character& chr);



} // namespace skyland
