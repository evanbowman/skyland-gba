////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    void update(Time delta);


    void rewind(Time delta);


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
