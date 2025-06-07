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

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class PhaseShifter final : public Room
{
public:
    PhaseShifter(Island* parent, const RoomCoord& position);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static const char* name()
    {
        return "phase-shifter";
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 1100.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 3};
    }


    static SystemString ui_name()
    {
        return SystemString::block_phase_shifter;
    }


    static Icon icon()
    {
        return 4216;
    }


    static Icon unsel_icon()
    {
        return 4200;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::accepts_ion_damage | RoomProperties::sylph_only;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override;


    Time reload_time_remaining() const override
    {
        return timer_;
    }


    void reset_state() override
    {
        activated_ = false;
        timer_ = 0;
    }


    void rewind_state(time_stream::event::PhaseMode p);


    void amplify(bool enabled) override;


    Time cooldown_interval() const;


    void finalize() override;


private:
    bool activated_ = false;
    bool loaded_ = false;
    Time timer_;
    bool amplify_ : 1 = false;
};



} // namespace skyland
