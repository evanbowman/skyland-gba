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
        move_blocks_help_promt_dont_remind_me,
    };

    enum class Difficulty : u8 {
        beginner,
        experienced,
        expert,
    } difficulty_ = Difficulty::beginner;

    Bitvector<128> hidden_rooms_;

    Bitvector<64> stateflags_;

    host_u64 achievement_flags_;
    host_u64 challenge_flags_;

    GlobalPersistentData()
    {
        stateflags_.set(developer_mode, false);
        achievement_flags_.set(0);
        challenge_flags_.set(0);
    }
};



struct PersistentData
{
    Coins coins_ = 0; // TODO: use HostInteger<> here?
    WorldGraph world_graph_;
    int current_world_location_ = 0;
    int zone_ = 0;

    HostInteger<u32> total_seconds_;
    HostInteger<u32> total_pauses_;
    HostInteger<s32> score_;
    HostInteger<s32> state_flags_;

    u8 lives_ = 2;

    enum StateFlag {
        workshop_built = (1 << 0),
        dev_mode_active = (1 << 1),
        opponent_crew_died = (1 << 2),
    };

    void set_flag(StateFlag flag)
    {
        state_flags_.set(state_flags_.get() | flag);
    }
};



} // namespace skyland
