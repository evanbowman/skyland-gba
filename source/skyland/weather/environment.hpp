////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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

#include "graphics/color.hpp"
#include "number/numeric.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"



class Platform;



namespace skyland
{
class App;
}



namespace skyland::weather
{



class Environment
{
public:
    virtual ~Environment()
    {
    }


    virtual void rewind_lightning()
    {
    }


    virtual bool is_cold() const
    {
        return false;
    }

    virtual void update(Microseconds delta) = 0;
    virtual void rewind(Microseconds delta) = 0;

    virtual void display() = 0;

    virtual const char* player_island_texture() const = 0;
    virtual const char* player_island_interior_texture() const = 0;

    virtual const char* opponent_island_texture() const = 0;
    virtual const char* opponent_island_interior_texture() const = 0;

    virtual const char* sprite_texture() const = 0;
    virtual const char* background_texture() const = 0;


    virtual ColorConstant fadein_colorize_tone() const
    {
        return ColorConstant::rich_black;
    }


    virtual void on_pause();


    virtual bool is_overcast() const
    {
        return false;
    }


    virtual Platform::Screen::Shader shader() const
    {
        return passthrough_shader;
    }


    virtual const char* music() const = 0;
};



class CleanEnvironment : public Environment
{
public:
    void update(Microseconds delta) override
    {
        // Nothing...
    }


    void rewind(Microseconds delta) override
    {
        // Nothing...
    }


    void display() override
    {
        // Nothing...
    }


    const char* player_island_texture() const override
    {
        return "tilesheet";
    }


    const char* player_island_interior_texture() const override
    {
        return "tilesheet_interior";
    }


    const char* opponent_island_texture() const override
    {
        return "tilesheet_enemy_0";
    }


    const char* opponent_island_interior_texture() const override
    {
        return "tilesheet_enemy_0_interior";
    }


    const char* sprite_texture() const override
    {
        return "spritesheet";
    }


    const char* background_texture() const override
    {
        return "background";
    }
};



class ClearSkies : public CleanEnvironment
{
public:
    const char* music() const override
    {
        return "life_in_silco";
    }


    ClearSkies()
    {
        timer_ = seconds(rng::choice<7>(rng::utility_state));
    }


    void update(Microseconds delta) override;


    void display() override;


    ColorConstant fadein_colorize_tone() const override;


private:
    Microseconds timer_;
};



} // namespace skyland::weather
