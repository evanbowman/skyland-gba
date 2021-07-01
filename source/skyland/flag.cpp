#include "flag.hpp"
#include "platform/platform.hpp"
#include "tile.hpp"



namespace skyland {



// Generate a wavy animation for the flag, and copy the tiles into vram.
void vram_write_flag(Platform& pfrm, const FlagPixels& px)
{
    u8 tile_data[16][16] = {0};


    for (int i = 0; i < FlagPixels::width; ++i) {
        for (int j = 0; j < FlagPixels::height; ++j) {
            tile_data[i][j + 1] = px.pixels[i][j];
        }
    }

    // Flag pole
    for (int j = 0; j < FlagPixels::height; ++j) {
        tile_data[13][j] = 1; // TODO...
    }


    pfrm.overwrite_t0_tile(Tile::flag_start, pfrm.encode_tile(tile_data));



    // Now, we want to generate our wavy animation...

    // TODO...

    pfrm.overwrite_t0_tile(Tile::flag_start + 1, pfrm.encode_tile(tile_data));

    // TODO...

    pfrm.overwrite_t0_tile(Tile::flag_start + 2, pfrm.encode_tile(tile_data));

    // TODO...

    pfrm.overwrite_t0_tile(Tile::flag_start + 3, pfrm.encode_tile(tile_data));
}



} // namespace skyland
