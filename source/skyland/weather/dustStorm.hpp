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
#include "storm.hpp"



namespace skyland::weather
{



class DustStorm : public ClearSkies
{
protected:
    ScratchMemory<State> state_;
    Time damage_timer_ = 0;

public:
    DustStorm();
    DustStorm(const DustStorm&) = delete;
    ~DustStorm();


    Platform::Screen::Shader shader() const override;


    void update(Time delta) override;


    void rewind(Time delta) override;


    void display() override;


    static const int id_ = 6;


    EnvironmentId id() const override;
};



} // namespace skyland::weather
