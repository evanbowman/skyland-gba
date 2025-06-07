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


#include "groupSelection.hpp"
#include "worldScene.hpp"



namespace skyland
{



class GroupSelectionScene : public ActiveWorldScene
{
public:
    GroupSelectionScene() = default;
    GroupSelectionScene(MetaclassIndex mti);


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    bool up_qd_ = false;
    bool down_qd_ = false;
    bool left_qd_ = false;
    bool right_qd_ = false;

private:
    void show_action_list();

    enum class State {
        draw_selection,
        list_actions,
        pick_group,
        animate_out,
    } state_ = State::draw_selection;

    u8 action_list_index_ = 0;
    u8 cursor_anim_frame_ = 0;
    Time cursor_anim_timer_ = 0;

    Time anim_out_timer_ = seconds(0);
    Fixnum anim_out_interval_ = 0.0_fixed;

    Vec2<Fixnum> last_draw_pos_[4];

    Optional<DynamicMemory<GroupSelection>> group_selection_;

    Optional<Text> text_;
    Optional<MetaclassIndex> group_mti_;
};



} // namespace skyland
