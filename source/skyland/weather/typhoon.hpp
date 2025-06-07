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



class Typhoon : public Storm
{
public:
    Platform::Screen::Shader shader() const override;


    void update(Time delta) override;
    void rewind(Time delta) override;


    void on_lightning() override;


    void rewind_lightning() override
    {
        ls_ = LightningState::fade;
        lightning_timer_ = 0;
    }


    ColorConstant fadein_colorize_tone() const override;


    EnvironmentId id() const override;


private:
    enum class LightningState : u8 {
        none,
        begin1,
        begin2,
        hold,
        fade,
    } ls_ = LightningState::none;

    Time lightning_timer_ = 0;
};



} // namespace skyland::weather
