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


    ScenePtr update(Time delta) override;


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
