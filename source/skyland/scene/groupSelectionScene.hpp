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


#include "groupSelection.hpp"
#include "worldScene.hpp"



namespace skyland
{



class GroupSelectionScene : public ActiveWorldScene
{
public:
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
};



} // namespace skyland
