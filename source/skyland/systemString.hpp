#pragma once

#include "bulkAllocator.hpp"



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
    block_reactor,
    block_replicator,
    block_shrubbery,
    block_speaker,
    block_stairwell,
    block_statue,
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

    dialog_tutorial_prompt,
};



using SystemStringBuffer = DynamicMemory<StringBuffer<1900>>;



SystemStringBuffer loadstr(Platform& pfrm, SystemString str);



// Just a shortcut to save myself from having to type this all out hundreds of
// times.
#define SYS_CSTR(TAG) loadstr(pfrm, SystemString::TAG)->c_str()
#define SYSTR(TAG) loadstr(pfrm, SystemString::TAG)



}
