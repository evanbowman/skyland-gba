////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



#pragma once

#include "environment.hpp"
#include "skyland/island.hpp"



namespace skyland::weather
{



class SolarStorm : public CleanEnvironment
{
public:
    SolarStorm()
    {
    }


    void update(Time delta) override;


    static constexpr const EnvironmentId id_ = 8;


    virtual EnvironmentId id() const override;


    Platform::Screen::Shader shader() const override;


    static Platform::Screen::Shader get_shader();


private:
    Time ignite_timer_ = 0;
};



} // namespace skyland::weather
