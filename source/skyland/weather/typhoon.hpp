////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
    Platform::Screen::Shader shader(App& app) const override;


    void update(App& app, Microseconds delta) override;
    void rewind(App& app, Microseconds delta) override;


    void on_lightning() override;


    void rewind_lightning() override
    {
        ls_ = LightningState::fade;
        lightning_timer_ = 0;
    }


    ColorConstant fadein_colorize_tone() const override;


private:
    enum class LightningState : u8 {
        none,
        begin1,
        begin2,
        hold,
        fade,
    } ls_ = LightningState::none;

    Microseconds lightning_timer_ = 0;
};



} // namespace skyland::weather
