////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "script/debug.hpp"



namespace skyland
{



lisp::debug::Action onscreen_script_debug_handler(lisp::debug::Interrupt irq,
                                                  lisp::Value* expr);



}
