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



namespace skyland::weather
{



struct State
{
    static const int particle_max = 12;

    Vec2<s16> raindrops_[particle_max];
    u8 sine_x_input_[particle_max];
    u8 sine_y_input_[particle_max];
    // NOTE: we already step through the sine table as slow as possible. In the
    // game's halfspeed mode, we need to toggle whether we increment the sine
    // input angle. Alternatively, I could use fixed point, but that seems
    // overkill here...
    bool sine_alt_ = false;
    int sine_damping_ = 1;
    Time thunder_timer_;
    Time lightning_timer_;

    int particle_count_ = 6;

    struct ColorTable
    {
        ColorConstant values_[16];
    };

    ColorTable t0_palette_;
    ColorTable t1_palette_;
    ColorTable background_palette_;
    ColorTable sprite_palette_;
    u16 spr_ = 89 * 8;

    Vec2<s16> last_camera_;

    State();

    void update(Time delta);
    void rewind(Time delta);
    void display();
};



class Storm : public CleanEnvironment
{
protected:
    ScratchMemory<State> state_;

public:
    Storm(int particle_count = 6);


    bool is_overcast() const override
    {
        return true;
    }


    void update(Time delta) override;


    void rewind(Time delta) override;


    void display() override;


    EnvironmentId id() const override;


    virtual void on_lightning()
    {
        // ...
    }


    Platform::Screen::Shader shader() const override;


    ColorConstant fadein_colorize_tone() const override;
};



} // namespace skyland::weather
