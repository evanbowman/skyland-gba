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
void draw_weapon_targets(const TargetQueue& tq);



} // namespace skyland::minimap
