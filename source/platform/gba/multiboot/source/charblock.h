
////////////////////////////////////////////////////////////////////////////////
//
// Tile Memory Layout:
//
// The game uses every single available screen block, so the data is fairly
// tightly packed. Here's a chart representing the layout:
//
// All units of length are in screen blocks, followed by the screen block
// indices in parentheses.
//
//     charblock 0        charblock 1      charblock 2
// ~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~
// o======================================================
// |    t0 texture    |   t1 texture   | overlay texture |
// |   len 8 (0 - 7)  | len 8 (8 - 15) | len 8 (16 - 23) | ...
// o======================================================
//
//                  charblock 3
//      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//     ====================================o
//     |     t0 mem      |     t1 mem      |
// ... | len 2 (26 - 27) | len 2 (28 - 29) | ...
//     ====================================o
//
//                        charblock 3 (contd.)
//      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//     ========================================================o
//     |   background texture    |  overlay mem  |   bg mem    |
// ... |    len 1 (24 - 25)      |  len 1 (30)   | len 1 (31)  |
//     ========================================================o
//
//


static const int sbb_per_cbb = 8; // ScreenBaseBlock per CharBaseBlock

static const int charblock_size = sizeof(ScreenBlock) * sbb_per_cbb;

static const int sbb_t0_tiles = 26;
static const int sbb_t1_tiles = 28;
static const int sbb_overlay_tiles = 30;
static const int sbb_bg_tiles = 31;

static const int sbb_background_texture = 24;
static const int cbb_background_texture =
    sbb_background_texture / sbb_per_cbb;

static const int sbb_overlay_texture = 16;
static const int sbb_t0_texture = 0;
static const int sbb_t1_texture = 8;

static const int cbb_overlay_texture =
    sbb_overlay_texture / sbb_per_cbb;

static const int cbb_t0_texture = sbb_t0_texture / sbb_per_cbb;
static const int cbb_t1_texture = sbb_t1_texture / sbb_per_cbb;
