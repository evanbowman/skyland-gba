////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


// The root class for all blocks! In practice, all derived block types inherit
// from the Room subclass, for historical reasons. I merely added the Block
// class because, concepturally, all Room instances should be blocks. If I had
// unlimited time, I'd refactor the Room class, moving most of the functionality
// to the block class. Room should really be reserved for structures that may be
// inhabited by characters, rather than the current hierarchy, where all blocks
// are rooms, some of which are not habitable. In practice, the code refers to
// variables called room in so many places, and the benefit to refactoring is
// mainly ideological rather than practical, so I haven't gotten around to it.
//
// Some history: Early in the developent, most blocks in skyland allowed
// characters to walk through them, and the game was more of a concept for an
// island building game. During a frantic development cycle for a contest, I
// added weapons and defenses, inheriting them from Room for simplicity.


namespace skyland
{



class Block
{
};



} // namespace skyland
