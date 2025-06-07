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


enum class Layer {
    overlay,
    // NOTE: map_0 and map_1 were used by the legacy blind jump core, before the
    // codebases diverged. Should not be used!
    map_1,
    map_0,
    background,
    map_1_ext,
    map_0_ext,
};
