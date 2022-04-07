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



#include "allocator.hpp"



namespace skyland
{



class App;
class Island;



using PathBuffer = Buffer<Vec2<u8>, 512>;
using Path = DynamicMemory<PathBuffer>;


std::optional<Path> find_path(Platform& pfrm,
                              App& app,
                              Island* island,
                              const Vec2<u8>& start,
                              const Vec2<u8>& end);



} // namespace skyland
