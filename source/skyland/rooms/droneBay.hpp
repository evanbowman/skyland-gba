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


#include "reconstructionQueue.hpp"
#include "skyland/coins.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class DroneBay final : public Room
{
public:
    DroneBay(Island* parent, const RoomCoord& position);


    void update(Time delta) override;
    void rewind(Time delta) override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;



    void display(Platform::Screen& screen) override;


    void finalize() override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static const char* name()
    {
        return "drone-bay";
    }


    static SystemString ui_name()
    {
        return SystemString::block_drone_bay;
    }


    static ATP atp_value()
    {
        return 1200.0_atp;
    }


    static void format_description(StringBuffer<512>& buffer);


    ScenePtr select_impl(const RoomCoord& cursor) override;


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 1032;
    }


    static Icon unsel_icon()
    {
        return 1016;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible |
               RoomProperties::human_only;
    }


    Optional<SharedEntityRef<Drone>> drone() const override
    {
        return drone_;
    }


    bool attach_drone(SharedEntityRef<Drone> drone) override;


    void detach_drone(bool quiet) override;


    Time reload_interval() const;


    void start_reload()
    {
        reload_ = reload_interval();
    }


    Time reload_time_remaining() const override
    {
        return reload_;
    }


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::vertical;
    }


    void on_powerchange() override;


    bool allows_powerdown() override;


    void parent_layout_changed(RoomCoord moved_from, RoomCoord to) override;


    void amplify(bool enable) override;


private:
    Optional<SharedEntityRef<Drone>> drone_;
    Time reload_ = 0;

public:
    ReconstructionQueue rq_;

private:
    bool amplify_ = false;
};


} // namespace skyland
