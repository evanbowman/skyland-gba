#pragma once

#include "bulkAllocator.hpp"



namespace skyland {



enum class SystemString {
    empty,
    game_title,
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
