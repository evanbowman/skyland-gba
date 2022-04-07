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
#include "bitvector.hpp"
#include "img.hpp"



namespace skyland
{



class CustomTileMapper
{
public:
    static const int mapping_count = 15;


    // returns -1 upon failure.
    int map_image(const img::Image& image);


    // Push all mapped images to video memory
    void publish_as_tiles(Platform& pfrm);

    void publish_as_sprites(Platform& pfrm);


    void clear()
    {
        mappings_.reset();
    }


private:
    struct Mappings
    {
        img::Image textures_[mapping_count];
        Bitvector<mapping_count> textures_in_use_;
    };

    std::optional<DynamicMemory<Mappings>> mappings_;
};



} // namespace skyland
