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
#include "skyland/systemString.hpp"



namespace skyland
{



class TargetingComputer final : public Room
{
public:
    TargetingComputer(Island* parent,
                      const RoomCoord& position,
                      const char* n = name());


    void update(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::accepts_ion_damage | RoomProperties::singleton;
    }


    bool description_visible() override
    {
        return true;
    }


    static ATP atp_value()
    {
        return 20.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "targeting-computer";
    }


    static SystemString ui_name()
    {
        return SystemString::block_targeting_computer;
    }


    static Icon icon()
    {
        return 3544;
    }


    static Icon unsel_icon()
    {
        return 3560;
    }


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


    void ___rewind___finished_reload() override;


    bool enabled() const
    {
        return enabled_;
    }


    void unset_target() override;


    void rescan()
    {
        rescan_ = true;
    }


private:
    Time next_action_timer_ = seconds(1);
    u32 room_update_index_ = 0;

    bool enabled_ = true;
    bool rescan_ = false;
};



} // namespace skyland
