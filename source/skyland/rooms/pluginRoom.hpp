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



namespace skyland
{


class PluginRoom : public Room
{
public:
    PluginRoom(Island* parent, const RoomCoord& position, RoomMeta* metaclass);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void render_interior(App* app, TileId buffer[16][16]);


    void render_exterior(App* app, TileId buffer[16][16]);


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void set_target(const RoomCoord& target, bool pinned) override;


    void unset_target() override;


    ScenePtr<Scene> select_impl(const RoomCoord& cursor) override;


    bool description_visible() override
    {
        return true;
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


private:
    Time timer_ = 0;
    Optional<RoomCoord> target_;
};



} // namespace skyland
