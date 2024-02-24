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
                         Optional<RoomCoord> initial_pos = {});


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    bool hide_chr_icon() const override;


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


    int selector_ = 0;

    Time describe_room_timer_ = milliseconds(400);
    Optional<Text> room_description_;

    u16 last_player_checksum_ = 0;
    u16 last_opponent_checksum_ = 0;

    Time minimap_repaint_timer_ = 0;

    int cursor_tics_ = 0;
    Time tic_timer_ = milliseconds(100);

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

    Optional<RoomCoord> initial_pos_;
};



} // namespace skyland
