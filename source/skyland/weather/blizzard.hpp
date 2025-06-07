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
#include "storm.hpp"



namespace skyland::weather
{



class Blizzard : public Storm
{
public:
    Blizzard() : Storm(12)
    {
        (*state_).sine_damping_ = 0;
    }


    Platform::Screen::Shader shader() const override;


    static Platform::Screen::Shader get_shader();


    bool is_cold() const override
    {
        return true;
    }


    EnvironmentId id() const override;


    void update(Time delta) override;
    void display() override;
};



} // namespace skyland::weather
