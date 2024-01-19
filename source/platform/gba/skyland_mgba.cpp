//
// This code is intended for interacting with a modded mgba program.
// Skyland.gba writes commands to a few custom registers, and receives responses
// from skyland-mgba.
//

#include "skyland_mgba.hpp"


#define REG_SKYLAND_MGBA_COMMAND (volatile u16*)0x4FFF598
#define REG_SKYLAND_MGBA_ARG (volatile u16*)0x4FFF596



ArgBuffer skyland_mgba_invoke_command(Command cmd, const ArgBuffer& arg)
{
    ArgBuffer result;

    for (char c : arg) {
        *REG_SKYLAND_MGBA_ARG = c;
    }

    *REG_SKYLAND_MGBA_COMMAND = (u16)cmd;

    auto result_count = (u8)*REG_SKYLAND_MGBA_COMMAND;
    for (int i = 0; i < result_count; ++i) {
        result.push_back(*REG_SKYLAND_MGBA_ARG);
    }

    return result;
}
