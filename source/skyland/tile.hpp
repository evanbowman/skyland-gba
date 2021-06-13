#pragma once


struct Tile {
    enum {
        terrain_middle = 12,
        terrain_left,
        terrain_right,
        wall_window_1,
        wall_window_2,
        wall_plain_1,
        wall_plain_2,
        roof_plain,
        roof_chimney,
        cannon_1,
        armored_wall_1,
        armored_wall_2,
        roof_plain_intersect_armored_wall,
        wall_window_middle_1,
        wall_window_middle_2,
        grass,
        reserved_1,
        hull,
        roof_flag = 109,
        flag = 110,
    };
};


struct InteriorTile {
    enum {
        core_1 = 15,
        core_2,
        core_3,
        core_4,
        roof_1,
        roof_2,
        cannon_1,
        ladder_base,
        ladder_mid,
        roof_3,
        ladder_mid_2,
        ladder_top,
        grass,
        empty,
        hull,
        workshop_1,
        workshop_2,
        workshop_3,
        workshop_4,
    };
};
