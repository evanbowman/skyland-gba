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

    block_arcgun,
    block_banana_plant,
    block_barrier,
    block_basalt,
    block_bridge,
    block_bronze_hull,
    block_cesium,
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
    block_forcefield,
    block_forcefield2,
    block_fountain,
    block_gold,
    block_hull,
    block_ice,
    block_infirmary,
    block_ion_cannon,
    block_ion_fizzler,
    block_lady_liberty,
    block_lava,
    block_lava_source,
    block_lemon_tree,
    block_manufactory,
    block_masonry,
    block_missile_silo,
    block_mycelium,
    block_nemesis,
    block_palm,
    block_piston,
    block_plundered_room,
    block_energized_hull,
    block_radar,
    block_radiator,
    block_reactor,
    block_replicator,
    block_shrubbery,
    block_solar_cell,
    block_speaker,
    block_stairwell,
    block_statue,
    block_sticky_piston,
    block_sunflower,
    block_switch,
    block_synth,
    block_dynamite_1,
    block_dynamite_2,
    block_torch,
    block_transporter,
    block_water,
    block_water_source,
    block_workshop,

    bird_label,

    character_label_human,
    character_label_goblin,
    character_label_replicant,

    menu_text_adventure,
    menu_text_challenge,
    menu_text_multiplayer,
    menu_text_extras,

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

    a_next,

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

    factory_reset,

    dialog_tutorial_prompt,

    multi_session_connecting,
    multi_connection_failure,

    misc_hibernate_message,
    misc_dlc_message,

    intro_sequence_message_1,
    intro_sequence_message_2,
    intro_sequence_message_3,

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
