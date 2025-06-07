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

#include "environment.hpp"
#include "skyland/island.hpp"
#include "typhoon.hpp"



namespace skyland::weather
{



class Dynamic : public Typhoon
{
public:
    Dynamic();


    void update(Time delta) override;


    void rewind(Time delta) override;


    Platform::Screen::Shader shader() const override;


    void compute_palettes(u8 scale);
};



} // namespace skyland::weather
