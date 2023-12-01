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



#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



extern SharedVariable decimator_reload_ms;



class Decimator final : public Room
{
public:
    Decimator(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void update(Microseconds delta) override;


    void rewind(Microseconds delta);


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void unset_target() override;


    void ___rewind___finished_reload() override;

    void ___rewind___ability_used() override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static Category category()
    {
        return Category::weapon;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "decimator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_decimator;
    }


    static Icon icon()
    {
        return 1272;
    }


    static Icon unsel_icon()
    {
        return 1288;
    }


    static ATP atp_value()
    {
        return 1500.0_atp;
    }


    bool description_visible() override
    {
        return true;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::roof_hidden | RoomProperties::locked_by_default |
               RoomProperties::habitable;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


    void finalize() override;


private:
    int counter_ = 0;


    Microseconds reload_ = 1000 * decimator_reload_ms;
};



} // namespace skyland
