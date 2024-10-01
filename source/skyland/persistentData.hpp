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



#include "achievement.hpp"
#include "bitvector.hpp"
#include "coins.hpp"
#include "flag.hpp"
#include "highscores.hpp"
#include "number/endian.hpp"
#include "worldGraph.hpp"



namespace skyland
{



struct GlobalPersistentData
{
    Highscores highscores_;

    enum Flags {
        developer_mode,
        tutorial_prompt,
        configured_clock,
        sandbox_prompt, // set by lisp script
        freebuild_unlocked,
        difficulty_prompt,
        save_prompt_dont_remind_me,
        datacarts_prompt,
        datacarts_help_prompt, // set by lisp script
        gamespeed_help_prompt_dont_remind_me,
        move_blocks_help_prompt_dont_remind_me,
        sel_menu_help_prompt_dont_remind_me,
        autofire_on,
        permadeath_on,
    };

    enum class Difficulty : u8 {
        beginner,
        experienced,
        expert,
    } difficulty_ = Difficulty::beginner;

    host_u64 watched_tutorials_;
    host_u64 unused_;

    Bitvector<64> stateflags_;

    host_u64 achievement_flags_;
    host_u64 challenge_flags_;

    GlobalPersistentData()
    {
        stateflags_.set(developer_mode, false);
        achievement_flags_.set(0);
        challenge_flags_.set(0);
        stateflags_.set(permadeath_on, true);
    }
};



struct PersistentData
{
    Coins coins_ = 0; // TODO: use HostInteger<> here?
    WorldGraph world_graph_;
    int current_world_location_ = 0;
    int zone_ = 0;

    HostInteger<u32> total_seconds_;
    HostInteger<u32> rng_;
    HostInteger<s32> score_;
    HostInteger<s32> state_flags_;

    u8 lives_ = 2;

    enum StateFlag {
        workshop_built = (1 << 0),
        dev_mode_active = (1 << 1),
        opponent_crew_died = (1 << 2),
        permadeath_on = (1 << 3),
    };

    void set_flag(StateFlag flag)
    {
        state_flags_.set(state_flags_.get() | flag);
    }

    void clear_flag(StateFlag flag)
    {
        state_flags_.set(state_flags_.get() & ~flag);
    }
};



} // namespace skyland
