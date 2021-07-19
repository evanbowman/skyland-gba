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
    for (int j = 0; j < 16; ++j) {
        tile_data[13][j] = 1; // TODO...
    }


    auto enc = pfrm.encode_tile(tile_data);

    pfrm.overwrite_t0_tile(Tile::flag_start, enc);
    pfrm.overwrite_t1_tile(Tile::flag_start, enc);



    // Now, we want to generate our wavy animation...

    for (int x = 9; x < 12; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }


    enc = pfrm.encode_tile(tile_data);

    pfrm.overwrite_t0_tile(Tile::flag_start + 1, enc);
    pfrm.overwrite_t1_tile(Tile::flag_start + 1, enc);



    for (int x = 9; x < 12; ++x) { // undo shift down
        for (int y = 1; y < 13; ++y) {
            tile_data[x][y] = tile_data[x][y + 1];
        }
    }


    for (int x = 5; x < 9; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }


    enc = pfrm.encode_tile(tile_data);

    pfrm.overwrite_t0_tile(Tile::flag_start + 2, enc);
    pfrm.overwrite_t1_tile(Tile::flag_start + 2, enc);




    for (int x = 5; x < 9; ++x) { // undo shift down
        for (int y = 1; y < 13; ++y) {
            tile_data[x][y] = tile_data[x][y + 1];
        }
    }

    for (int x = 0; x < 5; ++x) { // shift down
        for (int y = 12; y > 0; --y) {
            tile_data[x][y] = tile_data[x][y - 1];
        }
    }


    enc = pfrm.encode_tile(tile_data);

    pfrm.overwrite_t0_tile(Tile::flag_start + 3, enc);
    pfrm.overwrite_t1_tile(Tile::flag_start + 3, enc);
}



} // namespace skyland
