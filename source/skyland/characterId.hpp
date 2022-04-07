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

#include "number/numeric.hpp"



namespace skyland
{



// A globaly unique id, for uniquely identifying characters. I tried to get away
// without assigning ids to characters, but after dealing with bugs in obscure
// cases, I determined that characters need to be uniquely
// identifiable. Especially for rewinding history. Let's say that you want to
// rewind history faster than the original game speed. A character might be
// injured while walking, but when the game is rewound at double speed, the
// character might not actually be standing in the same position as it was when
// it was injured, so we need to use something other than a character's tile
// position to uniquely identify a character.
using CharacterId = u16;



} // namespace skyland
