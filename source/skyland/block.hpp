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
