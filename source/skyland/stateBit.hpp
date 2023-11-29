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
    count,
};



class App;



void state_bit_store(StateBit state_bit, bool value);



bool state_bit_load(StateBit state_bit);



using StateBitvector = Bitvector<(int)StateBit::count>;



} // namespace skyland
