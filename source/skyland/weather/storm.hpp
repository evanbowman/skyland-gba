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



namespace skyland::weather
{



class Storm : public CleanEnvironment
{
protected:
    Vec2<s16> last_camera_;

    struct State
    {
        Vec2<s16> raindrops_[6];
        Microseconds thunder_timer_;

        struct ColorTable
        {
            ColorConstant values_[16];
        };

        ColorTable t0_palette_;
        ColorTable t1_palette_;
        ColorTable background_palette_;
        ColorTable sprite_palette_;
    };


    ScratchMemory<State> state_;

public:
    Storm();


    bool is_overcast() const override
    {
        return true;
    }


    void update(Platform& pfrm, App& app, Microseconds delta);


    void rewind(Platform& pfrm, App& app, Microseconds delta);


    void display(Platform& pfrm, App& app) override;


    const char* music() const override;


    Platform::Screen::Shader shader(App& app) const override;
};



} // namespace skyland::weather
