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
