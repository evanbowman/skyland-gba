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



#include "allocator.hpp"
#include "coord.hpp"



namespace skyland
{



class App;
class Island;



using PathBuffer = Buffer<RoomCoord, 512>;
using Path = DynamicMemory<PathBuffer>;


class Character;


Optional<Path> find_path(Island* island,
                         Character* chr,
                         const RoomCoord& start,
                         const RoomCoord& end);



} // namespace skyland
