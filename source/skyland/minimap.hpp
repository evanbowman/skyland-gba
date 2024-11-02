////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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

#include "containers/vector.hpp"
#include "room.hpp"
#include "scene/groupSelection.hpp"



namespace skyland
{



class Weapon;



}



namespace skyland::minimap
{



struct FramebufferCache
{
    Vector<u8> pixels_;

    u16 player_island_checksum_ = -1;
    u16 opponent_island_checksum_ = -1;
};



struct Settings
{
    FramebufferCache* pixel_cache_ = nullptr;
    GroupSelection* weapon_group_selection_ = nullptr;
    Optional<RoomCoord> weapon_loc_ = nullopt();
    Optional<Room::Group> group_ = nullopt();
    Optional<bool> target_near_ = nullopt();

    bool show_destroyed_rooms_ = false;
};



void show();
void hide();
void move(u8 y_anchor);



void schedule_repaint();
bool needs_repaint();



extern Bitmatrix<16, 16> player_destroyed_rooms;



void repaint(const Settings& settings = {});
void draw_cursor(bool near);
void draw_weapon_targets(const Weapon& weapon);



} // namespace skyland::minimap
