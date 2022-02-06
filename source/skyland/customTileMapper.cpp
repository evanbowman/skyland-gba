#include "customTileMapper.hpp"
#include "tile.hpp"



namespace skyland {



int CustomTileMapper::map_image(Platform& pfrm, const img::Image& image)
{
    if (not mappings_) {
        mappings_ = allocate_dynamic<Mappings>(pfrm);
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
