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
    intro_credits_name,
    intro_credits_cpy,
};



using SystemStringBuffer = DynamicMemory<StringBuffer<1900>>;



SystemStringBuffer loadstr(Platform& pfrm, SystemString str);



// Just a shortcut to save myself from having to type this all out hundreds of
// times.
#define SYS_CSTR(TAG) loadstr(pfrm, SystemString::TAG)->c_str()
#define SYSTR(TAG) loadstr(pfrm, SystemString::TAG)


}
