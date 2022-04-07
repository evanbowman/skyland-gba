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


#include "customTileMapper.hpp"
#include "tile.hpp"



namespace skyland
{



int CustomTileMapper::map_image(const img::Image& image)
{
    if (not mappings_) {
        mappings_ = allocate_dynamic<Mappings>("custom-tile-mappings");
    }

    for (int i = 0; i < mapping_count; ++i) {
        if (not(*mappings_)->textures_in_use_[i]) {
            (*mappings_)->textures_in_use_.set(i, true);
            (*mappings_)->textures_[i] = image;
            return i;
        }
    }

    return -1;
}



void CustomTileMapper::publish_as_tiles(Platform& pfrm)
{
    if (not mappings_) {
        return;
    }

    u8 buffer[16][16];

    for (int i = 0; i < mapping_count; ++i) {
        if ((*mappings_)->textures_in_use_[i]) {
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    buffer[x][y] = (*mappings_)->textures_[i].get_pixel(x, y);
                }
            }

            pfrm.overwrite_t0_tile(Tile::dlc_tiles_begin + i,
                                   pfrm.encode_tile(buffer));

            pfrm.overwrite_t1_tile(Tile::dlc_tiles_begin + i,
                                   pfrm.encode_tile(buffer));
        }
    }
}



void CustomTileMapper::publish_as_sprites(Platform& pfrm)
{
    if (not mappings_) {
        return;
    }

    u8 buffer[16][16];

    for (int i = 0; i < mapping_count; ++i) {
        if ((*mappings_)->textures_in_use_[i]) {
            for (int x = 0; x < 16; ++x) {
                for (int y = 0; y < 16; ++y) {
                    buffer[x][y] = (*mappings_)->textures_[i].get_pixel(x, y);
                }
            }

            pfrm.overwrite_sprite_tile(SpriteTile::custom_sprite_tile_begin + i,
                                       pfrm.encode_tile(buffer));
        }
    }
}



} // namespace skyland
