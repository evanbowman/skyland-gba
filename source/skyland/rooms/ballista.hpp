////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2025  Evan Bowman. Some rights reserved.
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


#include "skyland/blockChecksum.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



class Ballista final : public Weapon
{
public:
    Ballista(Island* parent, const RoomCoord& position);


    void fire() override;
    Time reload_impl() const override;


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::horizontal;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::multiboot_compatible |
               RoomProperties::manufactory_required;
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
        return "ballista";
    }


    static SystemString ui_name()
    {
        return SystemString::block_ballista;
    }


    static ATP atp_value()
    {
        return 900.0_atp;
    }


    void plot_walkable_zones(bool matrix[16][16],
                             Character* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 4248;
    }


    static Icon unsel_icon()
    {
        return 4232;
    }

    Optional<u8> recalc_arc_height(const Vec2<Fixnum>& origin,
                                   const Vec2<Fixnum>& target) const;

    Vec2<Fixnum> emit_xy() const;
    Vec2<Fixnum> target_xy(RoomCoord c) const;

private:
    BlockChecksum arc_checksums_ = 0;
    Optional<RoomCoord> last_target_;
    Optional<u8> arc_height_;
};


} // namespace skyland
