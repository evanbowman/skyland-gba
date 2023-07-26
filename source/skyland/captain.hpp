////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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

#include "number/int.h"
#include "string.hpp"



namespace skyland
{



enum class CaptainAbility : u8 {
    warp,
    rate,
    range,
    terrain,
    none,
};



class BasicCharacter;
CaptainAbility captain_ability(BasicCharacter&);



void clear_captain_abilities();
void bind_captain_ability(CaptainAbility ability);



bool ability_active(CaptainAbility);



u16 captain_icon(CaptainAbility cap);



StringBuffer<64> captain_name(CaptainAbility cap);



StringBuffer<500> captain_desc(CaptainAbility cap);



class App;
void rebind_captain(App&);



} // namespace skyland
