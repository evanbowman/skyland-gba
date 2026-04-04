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

#include "number/numeric.hpp"



namespace skyland
{



bool script_preload_active();



struct ScriptPreloadGuard
{
    ScriptPreloadGuard();
    ~ScriptPreloadGuard();
    ScriptPreloadGuard(const ScriptPreloadGuard&) = delete;
};



using ElapsedTime = Time;
ElapsedTime preload_script_during_fade(Time fade_out_duration,
                                       const char* script_path);



} // namespace skyland
