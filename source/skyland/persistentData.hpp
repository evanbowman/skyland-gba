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



#include "achievement.hpp"
#include "bitvector.hpp"
#include "flag.hpp"
#include "highscores.hpp"
#include "number/endian.hpp"
#include "types.hpp"
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
        autofire_on, // deprecated
        permadeath_on,
        agb_color_mode,
        goblin_faction,
        sylph_faction,
        random_faction,
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



struct alignas(4) PersistentData
{
    PersistentData()
    {
        coins_.set(0);
        current_world_location_.set(0);
        zone_.set(0);
    }

    HostInteger<Coins> coins_;
    WorldGraph world_graph_;

    // NOTE: in the past, a couple of fields in persistent data were integers,
    // requiring specific padding and alignment. After switching to use the more
    // portable HostInteger<> class, we need to preserve alignment for
    // compatibility with old save files. But declaring the integers with a
    // fixed size and endianness was still necessary to enable save file
    // compatibility between GBA consoles and 64-bit desktop builds.
    u8 pad_[3];

    HostInteger<s32> current_world_location_;
    HostInteger<s32> zone_;

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
