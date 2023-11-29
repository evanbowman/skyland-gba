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
        static const int particle_max = 12;

        Vec2<s16> raindrops_[particle_max];
        Microseconds thunder_timer_;
        Microseconds lightning_timer_;

        int particle_count_ = 6;

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
    Storm(int particle_count = 6);


    bool is_overcast() const override
    {
        return true;
    }


    void update(Microseconds delta);


    void rewind(Microseconds delta);


    void display() override;


    const char* music() const override;


    virtual void on_lightning()
    {
        // ...
    }


    Platform::Screen::Shader shader() const override;


    ColorConstant fadein_colorize_tone() const override;
};



} // namespace skyland::weather
