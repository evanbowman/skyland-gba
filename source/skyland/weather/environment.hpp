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


    virtual void update(Platform& pfrm, App& app, Microseconds delta) = 0;
    virtual void rewind(Platform& pfrm, App& app, Microseconds delta) = 0;

    virtual void display(Platform& pfrm, App& app) = 0;

    virtual const char* player_island_texture() const = 0;
    virtual const char* player_island_interior_texture() const = 0;

    virtual const char* opponent_island_texture() const = 0;
    virtual const char* opponent_island_interior_texture() const = 0;

    virtual const char* sprite_texture() const = 0;
    virtual const char* background_texture() const = 0;


    virtual void on_pause();


    virtual bool is_overcast() const
    {
        return false;
    }


    virtual Platform::Screen::Shader shader(App& app) const
    {
        return passthrough_shader;
    }


    virtual const char* music() const = 0;
};



class CleanEnvironment : public Environment
{
public:
    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        // Nothing...
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        // Nothing...
    }


    void display(Platform& pfrm, App& app) override
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


    void update(Platform& pfrm, App& app, Microseconds delta) override;


private:
    Microseconds timer_;
};



} // namespace skyland::weather
