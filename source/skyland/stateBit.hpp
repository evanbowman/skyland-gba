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

#include "bitvector.hpp"



namespace skyland
{



enum class StateBit {
    surrender_offered,
    remote_console_force_newline,
    easy_mode_rewind_declined,
    crane_game_got_treasure,
    disable_autopause,
    successful_multiplayer_connect,
    multiboot,
    gamespeed_help_prompt,
    move_blocks_help_prompt,
    sel_menu_help_prompt,
    regression,
    show_fps,
    verbose_boot,
    minimap_on,
    count,
};



class App;



void state_bit_store(StateBit state_bit, bool value);



bool state_bit_load(StateBit state_bit);



using StateBitvector = Bitvector<(int)StateBit::count>;



} // namespace skyland
