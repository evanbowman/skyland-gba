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


#include "containers/vector.hpp"
#include "groupSelection.hpp"
#include "skyland/minimap.hpp"
#include "skyland/room.hpp"
#include "worldScene.hpp"



namespace skyland
{



class WeaponSetTargetScene : public ActiveWorldScene
{
public:
    WeaponSetTargetScene(GroupSelection& group);


    WeaponSetTargetScene(const RoomCoord& weapon_loc,
                         bool near = true,
                         Optional<RoomCoord> initial_pos = {});


    ScenePtr update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    bool hide_chr_icon() const override;


    bool displays_minimap() override;


private:
    // Store the tile coords of the room that we're setting the target for. If
    // we stored a pointer, we'd need to make all the room pointers into
    // shared/weak pointers instead of unique pointers, which we could easily
    // do, but doing so would use more memory.
    const RoomCoord weapon_loc_;


    void minimap_show();
    void minimap_hide();
    void minimap_init();
    void minimap_repaint();


    void snap();

    Optional<DynamicMemory<GroupSelection>> selection_;

    Time describe_room_timer_ = milliseconds(400);
    Optional<Text> room_description_;
    Optional<Text> target_queue_text_;

    void redraw_target_queue_text();

    u16 last_player_checksum_ = 0;
    u16 last_opponent_checksum_ = 0;

    Time minimap_repaint_timer_ = 0;

    int cursor_tics_ = 0;
    Time tic_timer_ = milliseconds(100);

    minimap::FramebufferCache fb_cache_;

    bool near_;
    bool resume_far_ = false;

    Room::Group group_ = Room::Group::none;
    u8 firing_mode_ = false;

    bool minimap_visible_ = false;
    u8 minimap_x_anchor_ = 1;

    Optional<RoomCoord> initial_pos_;

    TargetQueue target_queue_;
    bool queue_mode_ = false;
};



} // namespace skyland
