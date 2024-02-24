////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> select_impl(const RoomCoord& cursor) override;


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


    void begin_recharge()
    {
        recharge_ = 1000 * transporter_reload_ms;
    }


    void finalize() override;


private:
    Time recharge_ = 1000 * transporter_reload_ms;
};



void transport_character_impl(Island* src_island,
                              Island* dst_island,
                              CharacterId chr_id,
                              const RoomCoord& dst);



} // namespace skyland
