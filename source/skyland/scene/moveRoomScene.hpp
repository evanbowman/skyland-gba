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



void draw_required_space(App& app,
                         Island& island,
                         const Vec2<Fixnum> origin,
                         const Vec2<u8>& sz,
                         Room::WeaponOrientation o);



class MoveRoomScene : public ActiveWorldScene
{
public:
    MoveRoomScene(App& app, bool near)
    {
        bind_island(app, near);
    }


    void bind_island(App& app, bool near)
    {
        if (near) {
            island_ = &player_island(app);
            near_camera();
        } else {
            island_ = opponent_island(app);
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


    void exit(App& app, Scene& prev) override
    {
        ActiveWorldScene::exit(app, prev);

        text_.reset();
        no_text_.reset();
        yes_text_.reset();

        PLATFORM.fill_overlay(0);
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override;


    void display(App& app) override;


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
    Microseconds cursor_anim_timer_ = 0;

    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;

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

    std::optional<DynamicMemory<GroupSelection>> group_selection_;

    Island* island_;
};



} // namespace skyland
