////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "preload.hpp"
#include "stateBit.hpp"



namespace skyland
{



ScriptPreloadGuard::ScriptPreloadGuard()
{
    if (script_preload_active()) {
        LOGIC_ERROR();
    }
    state_bit_store(StateBit::script_preload_active, true);
}



ScriptPreloadGuard::~ScriptPreloadGuard()
{
    state_bit_store(StateBit::script_preload_active, false);
}




bool script_preload_active()
{
    return state_bit_load(StateBit::script_preload_active);
}



}
