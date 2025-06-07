////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "containers/vector.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



struct UserContext
{
    Optional<Vector<char>> yank_buffer_;
    Optional<DeferredScene> browser_exit_scene_;
    u8 hide_path_ = 0;
    bool allow_backtrack_ = true;
    bool readonly_ = false;
};



} // namespace skyland
