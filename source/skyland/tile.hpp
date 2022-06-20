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


#pragma once


#include "tileId.hpp"



namespace skyland
{



struct StaticTile
{
    // These tiles are not dynamically re-mappable. They live at fixed positions
    // at the beginning of the tile set.
    enum __StaticTile : TileId {
        null,
        airborne_selection = 1,
        flag_start = 2,
        flag_end = 5,
        group_icons,
        path_marker,
        count,
    };
};



struct Tile
{
    enum __Tile : TileId {
        null,
        airborne_selection = 1,
        flag_start = 2,
        flag_end = 5,
        __fixme,
        __fixme2,
        palm_1,
        palm_2,
        shrubbery,
        ion_fizzler_exterior = 11,
        terrain_middle,
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
        wall_plain_middle,
        reserved_3,
        reserved_4,
        reserved_5,
        reserved_6,
        missile_silo_1,
        missile_silo_2,
        forcefield,
        reserved7,
        reserved8,
        reserved9,
        reserved10,
        tin_chimney,
        plundered_room,
        reserved_11,
        reserved_12,
        radar_1,
        radar_2,
        reserved_13,
        reserved_14,
        reserved_15,
        reserved_16,
        particle_gun,
        reserved_17,
        reserved_18,
        reserved_19,
        reserved_20,
        reserved_21,
        flak_gun_1,
        flak_gun_2,
        drone_bay_1,
        drone_bay_2,
        decimator_1,
        decimator_2,
        reserved_22,
        reserved_23,
        reserved_24,
        reserved_25,
        reserved_26,
        field_hull,
        ion_fizzler,
        reserved_27,
        reserved_28,
        reserved_29,
        reserved_30,
        reserved_31,
        reserved_32,
        arc_gun,
        masonry,
        statue_1,
        statue_2,
        bridge,
        nemesis_1,
        nemesis_2,
        bronze,
        dynamite,
        tnt,
        reserved_33,
        synth,
        speaker_inactive,
        speaker_active,
        radiator,
        switch_1,
        switch_on,
        switch_off,
        banana_plant,
        roof_flag,
        flag_mount,
        sunflower,
        fountain,
        bridge_truss,
        scaffolding_angled_l,
        scaffolding_angled_r,
        strut,
        strut_top,
        roof_strut,
        roof_strut_joined,
        soil,
        mirror_hull,
        stacked_hull,
        lemon_tree_1,
        lemon_tree_2,

        RESERVED_RANGE_DO_NOT_USE,

        dlc_tiles_begin = 113,
        dlc_tiles_end = 127,

        liberty_1,
        liberty_2,
        liberty_3,
        liberty_4,
        liberty_5,
        liberty_6,
        liberty_7,
        liberty_8,
        escape_beacon_off_1,
        escape_beacon_off_2,
        escape_beacon_base,
        escape_beacon_on_1,
        escape_beacon_on_2,
        barrier,
        water_top,
        water_right,
        water_column,
        water_left,
        lava_top,
        lava_right,
        lava_column,
        lava_left,
        mycelium,
        mycelium_top,
        mycelium_middle,
        mycelium_bottom,
        piston_closed_r,
        piston_opened_r_1,
        piston_opened_r_2,
        piston_closed_l,
        piston_opened_l_1,
        piston_opened_l_2,
        piston_closed_u,
        piston_opened_u_1,
        piston_opened_u_2,
        piston_closed_d,
        piston_opened_d_1,
        piston_opened_d_2,
        forcefield2,
        solar_cell,
        fire_charge_1,
        fire_charge_2,
        ice,
        basalt_top,
        basalt,
        torch,
        cesium,
        gold,
        rocket_silo_1,
        rocket_silo_2,
        rocket_silo_3,
        crane_1,
        crane_2,
        crane_3,
        crane_4,
        crane_5,
        crane_6,
    };
};



struct InteriorTile
{
    enum __InteriorTile : TileId {
        null,
        airborne_selection = 1,
        flag_start = 2,
        flag_end = 5,
        __fixme,
        __fixme2,
        palm_1,
        palm_2,
        shrubbery,
        ion_fizzler_interior = 11,
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
        __unused__,
        ladder_top,
        grass,
        empty,
        hull,
        workshop_1,
        workshop_2,
        workshop_3,
        workshop_4,
        __unused2__,
        missile_silo_1,
        missile_silo_2,
        forcefield,
        infirmary_1,
        infirmary_3,
        infirmary_2,
        infirmary_4,
        tin_chimney,
        plundered_room,
        transporter_1,
        transporter_2,
        radar_1,
        radar_2,
        bulkhead_closed_1,
        bulkhead_closed_2,
        bulkhead_open_1,
        plain_floor,
        particle_gun,
        transporter_recharge,
        replicator_1,
        replicator_2,
        replicator_3,
        replicator_4,
        flak_gun_1,
        flak_gun_2,
        drone_bay_1,
        drone_bay_2,
        decimator_1,
        decimator_2,
        decimator_int,
        reactor_1,
        reactor_2,
        reactor_3,
        reactor_4,
        field_hull,
        ion_fizzler,
        manufactory_1,
        manufactory_2,
        manufactory_3,
        manufactory_4,
        manufactory_5,
        cargo_bay,
        arc_gun,
        masonry,
        statue_1,
        statue_2,
        bridge,
        nemesis_1,
        nemesis_2,
        bronze,
        dynamite,
        tnt,
        __unused3__,
        synth,
        speaker_inactive,
        speaker_active,
        radiator,
        switch_1,
        switch_on,
        switch_off,
        banana_plant,
        roof_flag,
        flag_mount,
        sunflower,
        fountain,
        bridge_truss,
        scaffolding_angled_l,
        scaffolding_angled_r,
        strut,
        strut_top,
        roof_strut,
        roof_strut_joined,
        soil,
        mirror_hull,
        stacked_hull,
        lemon_tree_1,
        lemon_tree_2,

        // NOTE: Tile 112 displays some "out of memory" text. But, in practice,
        // it's actually really difficult to run out of memory for unique tiles,
        // given the size of the level maps. I tried to use up all tile vram,
        // and couldn't produce an error. Basically, we have
        // one-hundred-and-three or so dynamically loaded tile graphics slots on
        // the gba. The islands in the game support ten blocks in the
        // y-direction and thirteen blocks in the x-direction. So the levels
        // only allow 130 unique blocks (tiles) in the first place. In practice,
        // you'd basically need to intentionally try to produce an error,
        // because no sane playing style would involve filling the level map
        // with 130 different blocks. Furthermore, some blocks have energy
        // requirements, so, at best, you'd need to build several copies of a
        // reactor, probably at least two. So subtract five blocks from the
        // total (reactor tile count). The game's code does handle out of memory
        // errors for vram without crashing, it should in theory just display a
        // tile with a little error message printed on it. But I haven't managed
        // to test whether the code for handling vram out of mem scenarios
        // actually works.
        RESERVED_RANGE_DO_NOT_USE,

        dlc_tiles_begin = 113,
        dlc_tiles_end = 127,

        liberty_1,
        liberty_2,
        liberty_3,
        liberty_4,
        liberty_5,
        liberty_6,
        liberty_7,
        liberty_8,
        escape_beacon_off_1,
        escape_beacon_off_2,
        escape_beacon_base,
        escape_beacon_on_1,
        escape_beacon_on_2,
        barrier,
        water_top,
        water_right,
        water_column,
        water_left,
        lava_top,
        lava_right,
        lava_column,
        lava_left,
        mycelium,
        mycelium_top,
        mycelium_middle,
        mycelium_bottom,
        piston_closed_r,
        piston_opened_r_1,
        piston_opened_r_2,
        piston_closed_l,
        piston_opened_l_1,
        piston_opened_l_2,
        piston_closed_u,
        piston_opened_u_1,
        piston_opened_u_2,
        piston_closed_d,
        piston_opened_d_1,
        piston_opened_d_2,
        forcefield2,
        solar_cell,
        fire_charge_1,
        fire_charge_2,
        ice,
        basalt_top,
        basalt,
        torch,
        cesium,
        gold,
        rocket_silo_1,
        rocket_silo_2,
        rocket_silo_3,
        crane_1,
        crane_2,
        crane_3,
        crane_4,
        crane_5,
        crane_6,
    };
};


struct SpriteTile
{
    enum {
        custom_sprite_tile_begin = 110,
    };
};



} // namespace skyland
