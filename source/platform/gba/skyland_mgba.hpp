#pragma once


#include "string.hpp"



using ArgBuffer = StringBuffer<64>;



enum class Command : u16 {
    identify = 1,
    button_mappings = 2,
    achievement = 3,
};



ArgBuffer skyland_mgba_invoke_command(Command cmd, const ArgBuffer& arg);
