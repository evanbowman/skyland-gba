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


#include "constructionScene.hpp"
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/island.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene/notificationScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "worldScene.hpp"



namespace skyland
{



void draw_required_space(Island& island,
                         const Vec2<Fixnum> origin,
                         const Vec2<u8>& sz,
                         Room::WeaponOrientation o);



class MoveRoomScene : public ActiveWorldScene
{
public:
    MoveRoomScene(bool near)
    {
        bind_island(near);
    }


    void bind_island(bool near)
    {
        if (near) {
            island_ = &player_island();
            near_camera();
        } else {
            island_ = opponent_island();
            far_camera();
        }
    }


    auto& cursor()
    {
        if (is_far_camera()) {
            return globals().far_cursor_loc_;
        } else {
            return globals().near_cursor_loc_;
        }
    }


    void exit(Scene& prev) override
    {
        ActiveWorldScene::exit(prev);

        text_.reset();
        no_text_.reset();
        yes_text_.reset();

        PLATFORM.fill_overlay(0);
    }


    ScenePtr<Scene> update(Time delta) override;


    void display() override;


private:
    enum class State {
        setup_prompt,
        prompt,
        move_stuff,
        move_block,
        select_group,
        move_group,
    } state_ = State::setup_prompt;

    Vec2<u8> move_diff_;
    u8 cursor_anim_frame_ = 0;
    Time cursor_anim_timer_ = 0;

    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;

    RoomCoord move_src_;
    Vec2<u8> mv_size_;
    Room::WeaponOrientation mv_ot_;

    struct GroupSelection
    {
        RoomCoord sel_tl_;
        RoomCoord sel_bl_;
        RoomCoord sel_tr_;
        RoomCoord sel_br_;

        RoomCoord anchor_;

        Buffer<RoomCoord, 90> rooms_;
    };

    Optional<DynamicMemory<GroupSelection>> group_selection_;

    Island* island_;
};



} // namespace skyland
