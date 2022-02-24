#pragma once

#include "bulkAllocator.hpp"



// All strings required by the engine. We store the strings themselves in a
// separate file, for localization purposes.



namespace skyland {



enum class SystemString {
    empty,
    game_title,

    block_arcgun,
    block_bridge,
    block_bronze_hull,
    block_bulkhead_door,
    block_cannon,
    block_cargo_bay,
    block_power_core,
    block_decimator,
    block_drone_bay,
    block_flak_gun,
    block_forcefield,
    block_hull,
    block_infirmary,
    block_ion_cannon,
    block_ion_fizzler,
    block_manufactory,
    block_masonry,
    block_missile_silo,
    block_nemesis,
    block_palm,
    block_plundered_room,
    block_energized_hull,
    block_radar,
    block_radiator,
    block_reactor,
    block_replicator,
    block_shrubbery,
    block_speaker,
    block_stairwell,
    block_statue,
    block_switch,
    block_synth,
    block_dynamite_1,
    block_dynamite_2,
    block_transporter,
    block_workshop,

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

    intro_credits_name,
    intro_credits_cpy,

    description_power_core,
    description_missile_silo,
    description_workshop,
    description_radar,
    description_ion_fizzler,
    description_speaker,
    description_shrubbery,
    description_ion_cannon,
    description_arc_gun,
    description_bronze_hull,
    description_nemesis,
    description_hull,
    description_forcefield,
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
    start_menu_save_sandbox,
    start_menu_load_sandbox,
    start_menu_quit,
    start_menu_sky_map,

    salvage_prompt,
    salvage_option_A,
    salvage_option_B,
    salvage_error_populated,
    salvage_drone,

    achievement_msg_title,
    achievement_msg_unlocked,

    weapon_group_prompt,

    deploy_drone_prompt,
    drone_position_prompt,

    highscores_title,
    highscores_score,

    modifier_keys_title,
    modifier_keys_opt_1,
    modifier_keys_opt_2,
    modifier_keys_opt_3,
    modifier_keys_opt_4,

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
    yes,
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

    dialog_tutorial_prompt,

    multi_session_connecting,
    multi_connection_failure,

    misc_hibernate_message,
    misc_dlc_message,
};



using SystemStringBuffer = DynamicMemory<StringBuffer<1900>>;



SystemStringBuffer loadstr(Platform& pfrm, SystemString str);



// Just a shortcut to save myself from having to type this all out hundreds of
// times.
#define SYS_CSTR(TAG) loadstr(pfrm, SystemString::TAG)->c_str()
#define SYSTR(TAG) loadstr(pfrm, SystemString::TAG)



} // namespace skyland
