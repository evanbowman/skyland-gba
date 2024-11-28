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
                             BasicCharacter* for_character) override
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
               RoomProperties::multiboot_compatible;
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
