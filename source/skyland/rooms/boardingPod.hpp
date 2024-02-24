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



class BoardingPod final : public Room
{
public:
    BoardingPod(Island* parent, const RoomCoord& position);


    void update(Time delta) override;
    void rewind(Time delta) override;


    static void format_description(StringBuffer<512>& buffer);

    void apply_damage(Health damage, Island* source) override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static ATP atp_value()
    {
        return 800.0_atp;
    }


    static Vec2<u8> size()
    {
        return {2, 3};
    }


    static const char* name()
    {
        return "boarding-pod";
    }


    void render_scaffolding(TileId buffer[16][16]) override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::roof_hidden |
               RoomProperties::disallow_chimney |
               RoomProperties::destroy_quietly |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::multiplayer_unsupported |
               RoomProperties::locked_by_default;
    }


    static SystemString ui_name()
    {
        return SystemString::block_boarding_pod;
    }


    static Icon icon()
    {
        return 3960;
    }


    static Icon unsel_icon()
    {
        return 3944;
    }


    bool description_visible() override
    {
        return true;
    }


    void set_target(const RoomCoord& target, bool pinned) override;


    void unset_target() override;


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor) override
    {
    }


    Optional<RoomCoord> get_target() const override
    {
        return target_;
    }


    ScenePtr<Scene> select(const RoomCoord& cursor) override;


    Island* owner_ = nullptr;


    Island* owner() const
    {
        return owner_;
    }


private:
    Optional<RoomCoord> target_;
    Time launch_timer_ = 0;
    Time heal_timer_ = 0;
};



} // namespace skyland
