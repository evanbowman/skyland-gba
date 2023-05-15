////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "containers/vector.hpp"
#include "skyland/room.hpp"
#include "worldScene.hpp"



namespace skyland
{



class WeaponSetTargetScene : public ActiveWorldScene
{
public:
    WeaponSetTargetScene(const RoomCoord& weapon_loc,
                         bool near = true,
                         std::optional<RoomCoord> initial_pos = {});


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


private:
    // Store the tile coords of the room that we're setting the target for. If
    // we stored a pointer, we'd need to make all the room pointers into
    // shared/weak pointers instead of unique pointers, which we could easily
    // do, but doing so would use more memory.
    const RoomCoord weapon_loc_;


    void minimap_show(Platform&, App&, u8 anchor);
    void minimap_hide(Platform&, App&);
    void minimap_init(Platform&, App&);
    void minimap_repaint(Platform&, App&);


    void snap(Platform&, App&);


    int selector_ = 0;

    Microseconds describe_room_timer_ = milliseconds(400);
    std::optional<Text> room_description_;

    u16 last_player_checksum_ = 0;
    u16 last_opponent_checksum_ = 0;

    Microseconds minimap_repaint_timer_ = 0;

    int cursor_tics_ = 0;
    Microseconds tic_timer_ = milliseconds(100);

    struct MinimapFramebufferCache
    {
        Vector<u8> pixels_;

        u16 player_island_checksum_ = -1;
        u16 opponent_island_checksum_ = -1;
    } fb_cache_;

    bool near_;
    bool resume_far_ = false;

    Room::Group group_ = Room::Group::none;
    u8 firing_mode_ = false;

    bool minimap_visible_ = false;
    u8 minimap_x_anchor_ = 1;

    std::optional<RoomCoord> initial_pos_;
};



} // namespace skyland
