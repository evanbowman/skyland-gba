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

#include "allocator.hpp"



// All strings required by the engine. We store the strings themselves in a
// separate file, for localization purposes.



namespace skyland
{



void systemstring_drop_index_cache();



void systemstring_bind_file(const char* path);



enum class SystemString {
    empty,
    game_title,

    yes,
    no,

    mt_title,
    mt_hint,
    mt_co_op,
    mt_vs,
    mt_game_mode,
    mt_prep_seconds,
    mt_unhide_prep,
    mt_coins,
    mt_terrain_size,
    mt_waiting,

    block_air,
    block_annihilator,
    block_arcgun,
    block_arch,
    block_banana_plant,
    block_barrier,
    block_basalt,
    block_basalt_brick,
    block_basalt_carved,
    block_bridge,
    block_bronze_hull,
    block_building,
    block_carved_masonry,
    block_cocoa,
    block_corn,
    block_cesium,
    block_crane,
    block_carved_crystal,
    block_crystal,
    block_crystal_pillar,
    block_stacked_hull,
    block_mirror_hull,
    block_bulkhead_door,
    block_cannon,
    block_cargo_bay,
    block_power_core,
    block_decimator,
    block_drone_bay,
    block_escape_beacon,
    block_flak_gun,
    block_fire_charge,
    block_food,
    block_forcefield,
    block_forcefield2,
    block_fountain,
    block_gold,
    block_harbor,
    block_honey,
    block_hull,
    block_ice,
    block_indigo,
    block_infirmary,
    block_ion_cannon,
    block_ion_fizzler,
    block_lady_liberty,
    block_lava,
    block_lava_source,
    block_lemon_tree,
    block_light_source,
    block_lumber,
    block_madder,
    block_manufactory,
    block_marble,
    block_masonry,
    block_missile_silo,
    block_mycelium,
    block_nemesis,
    block_palm,
    block_pearls,
    block_piston,
    block_plundered_room,
    block_potatoes,
    block_energized_hull,
    block_radar,
    block_radiator,
    block_reactor,
    block_replicator,
    block_road_ns,
    block_road_we,
    block_rocket_bomb,
    block_saffron,
    block_sand,
    block_scaffolding,
    block_shellfish,
    block_shrubbery,
    block_singularity,
    block_solar_cell,
    block_speaker,
    block_stairwell,
    block_statue,
    block_stone_pillar,
    block_sticky_piston,
    block_sunflower,
    block_switch,
    block_synth,
    block_tea,
    block_terrain,
    block_tulips,
    block_dynamite_1,
    block_dynamite_2,
    block_torch,
    block_transporter,
    block_volcanic_soil,
    block_water,
    block_water_source,
    block_wheat,
    block_windmill,
    block_wool,
    block_workshop,

    red,
    black,

    bird_label,

    character_label_human,
    character_label_goblin,
    character_label_replicant,

    menu_text_adventure,
    menu_text_challenge,
    menu_text_multiplayer,
    menu_text_extras,
    menu_text_macro,

    module_achievements,
    module_credits,
    module_update_loader,
    module_dlc_manager,
    module_factory_reset,
    module_file_browser,
    module_flag_designer,
    module_glossary,
    module_sandbox,
    module_skyland_forever,
    module_text_editor,
    module_highscores,
    module_tutorials,
    module_developer_mode,
    module_macro,
    module_checkers,

    developer_mode_msg,
    developer_mode_msg_2,

    cargo,

    none,

    construction_build,
    construction_add_terrain,
    construction_insufficient_funds,
    construction_insufficient_power_supply,
    construction_too_many_rooms,

    category_begin,
    category_wall = category_begin,
    category_weapon,
    category_factory,
    category_power,
    category_misc,
    category_decoration,


    intro_credits_name,
    intro_credits_cpy,

    macro_year,
    macro_create_block,
    macro_build_improvement,
    macro_demolish,
    macro_rotate,
    macro_raise,
    macro_layers,
    macro_day_or_night,
    macro_visible_layers,
    macro_next_turn,
    macro_budget,
    macro_citizens,
    macro_enter,
    macro_create_colony,
    macro_fullsize_colony,
    macro_outpost_colony,
    macro_set_name,
    macro_colony_resources,
    macro_colony_cost,
    macro_export,
    macro_export_how_many,
    macro_export_where,
    macro_commodities,
    macro_trade,
    macro_abandon,
    macro_rename_island,

    macro_fiscal_budget,
    macro_fiscal_employed,
    macro_fiscal_unemployed,
    macro_fiscal_homelessness,
    macro_fiscal_total,
    macro_fiscal_starvation,
    macro_fiscal_happiness,
    macro_fiscal_unhappiness,

    macro_food_supply,
    macro_housing_scarcity,
    macro_population_density,
    macro_total_happiness,

    macro_cube,
    macro_pancake,
    macro_pillar,


    description_annihilator,
    description_crane,
    description_power_core,
    description_solar_cell,
    description_fire_charge,
    description_missile_silo,
    description_water,
    description_water_source,
    description_workshop,
    description_radar,
    description_ion_fizzler,
    description_lady_liberty,
    description_lava,
    description_lava_source,
    description_lemon_tree,
    description_speaker,
    description_shrubbery,
    description_ion_cannon,
    description_arc_gun,
    description_bronze_hull,
    description_stacked_hull,
    description_mirror_hull,

    description_nemesis,
    description_hull,
    description_forcefield,
    description_forcefield2,
    description_reactor,
    description_bulkhead_door,
    description_transporter,
    description_energized_hull,
    description_flak_gun,
    description_manufactory,
    description_masonry,
    description_decimator,
    description_stairwell,
    description_plundered_room,
    description_synth,
    description_cargo_bay,
    description_cannon,
    description_radiator,
    description_switch,
    description_banana_plant,
    description_barrier,
    description_escape_beacon,
    description_mycelium,
    description_ice,
    description_basalt,
    description_torch,
    description_cesium,
    description_gold,
    description_rocket_bomb,

    gs_paused,
    gs_slow,
    gs_regular,
    gs_fast,
    gs_rewind,
    gs_error,
    gs_prompt,

    start_menu_resume,
    start_menu_glossary,
    start_menu_hibernate,
    start_menu_disable_rooms,
    start_menu_save_sandbox,
    start_menu_load_sandbox,
    start_menu_quit,
    start_menu_scuttle,
    start_menu_sky_map,
    start_menu_hint,
    start_menu_sandbox_help,
    start_menu_save,
    start_menu_load,
    start_menu_macroverse,
    start_menu_next_turn,
    start_menu_adjust_view,
    start_menu_newgame,
    start_menu_link,
    start_menu_share,

    macro_share_please_wait,

    salvage_prompt,
    salvage_option_A,
    salvage_option_B,
    salvage_error_populated,
    salvage_error_disallowed,
    salvage_drone,

    achievement_msg_title,
    achievement_msg_unlocked,

    weapon_group_prompt,

    deploy_drone_prompt,
    drone_position_prompt,

    highscores_title,
    highscores_score,
    highscores_upload,
    highscores_leaderboard,

    highscores_scan_qr_leaderboard,

    score_upload_enter_token,
    score_upload_prompt_1,
    score_upload_prompt_2,
    score_upload_prompt_3,

    a_next,
    qr_prep,

    modifier_keys_title,
    modifier_keys_opt_1,
    modifier_keys_opt_2,
    modifier_keys_opt_3,
    modifier_keys_opt_4,
    modifier_keys_opt_5,

    key_combo_prompt,

    level_complete_time,
    level_complete_pauses,
    level_complete_coins,
    level_complete_rooms,

    transporter_transport_char,
    transporter_recover_char,

    repair_range,

    create_replicant,

    reset_sandbox_query,
    exit,

    switch_connect_on,
    switch_connect_off,

    zone_text,

    wg_visited,
    wg_neutral,
    wg_hostile,
    wg_storm,
    wg_quest,
    wg_outpost,
    wg_uncharted,
    wg_quest_marker,
    wg_saved,
    wg_title,
    wg_storm_label,
    wg_exit,

    sandbox_coins,
    sandbox_terrain_size,
    sandbox_music,
    sandbox_building_dependencies,
    sandbox_weather,
    sandbox_title,
    sandbox_prompt,
    sandbox_characters,

    sf_description,
    sf_hint,
    sf_title,
    sf_difficulty,
    sf_casual,
    sf_normal,
    sf_hard,

    achievement_builder_name,
    achievement_builder_description,
    achievement_architect_name,
    achievement_architect_description,
    achievement_architect2_name,
    achievement_architect2_description,
    achievement_explorer_name,
    achievement_explorer_description,
    achievement_strategist_name,
    achievement_strategist_description,
    achievement_stronghold_name,
    achievement_stronghold_description,
    achievement_dynamite_name,
    achievement_dynamite_description,
    achievement_maestro1_name,
    achievement_maestro1_description,
    achievement_maestro2_name,
    achievement_maestro2_description,
    achievement_triage_name,
    achievement_triage_description,
    achievement_banana_man_name,
    achievement_banana_man_description,
    achievement_ancient_weapon_name,
    achievement_ancient_weapon_description,
    achievement_ship_of_theseus_name,
    achievement_ship_of_theseus_description,
    achievement_lemons_name,
    achievement_lemons_description,
    achievement_new_colossus_name,
    achievement_new_colossus_description,
    achievement_meltdown_name,
    achievement_meltdown_description,
    achievement_completionist_name,
    achievement_completionist_description,
    achievement_mycelium_name,
    achievement_mycelium_description,
    achievement_primitive_name,
    achievement_primitive_description,

    options,

    hint_title,
    hint_gamespeed,
    hint_infirmary,
    hint_navigation,
    hint_doors,
    hint_hotkeys,
    hint_glossary,
    hint_plunder,
    hint_damaged_core,
    hint_tutorials,

    no_dlc_prompt,
    dlc_erase_hint,

    piston_setup,
    piston_error,

    error_power_out,
    error_friendly,
    error_no_more_pauses,

    easy_mode_auto_rewind_title,
    easy_mode_auto_rewind_text,

    factory_reset,

    dialog_tutorial_prompt,
    adventure_completed_message,

    multi_session_connecting,
    multi_connection_failure,

    misc_hibernate_message,
    misc_dlc_message,

    intro_sequence_message_1,
    intro_sequence_message_2,
    intro_sequence_message_3,

    grav_collapse_started,
    grav_collapse_ended,

    freebuild_locked_text,

    qr_code_size_error,
    qr_code_size_warning,

    checkers_ai_thinking,
    checkers_forced_jump,
    checker_wins,
    checkers_jump_again,

    __NOTICE__,
    patchfix_retain_alphabet,
    count,
};



using SystemStringBuffer = DynamicMemory<StringBuffer<1900>>;



SystemStringBuffer loadstr(Platform& pfrm, SystemString str);



// Just a shortcut to save myself from having to type this all out hundreds of
// times.
#define SYS_CSTR(TAG) loadstr(pfrm, SystemString::TAG)->c_str()
#define SYSTR(TAG) loadstr(pfrm, SystemString::TAG)



} // namespace skyland
