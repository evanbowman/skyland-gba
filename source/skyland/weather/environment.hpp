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

#include "graphics/color.hpp"
#include "number/numeric.hpp"
#include "number/random.hpp"
#include "platform/conf.hpp"
#include "platform/platform.hpp"



class Platform;



namespace skyland
{
class App;



using EnvironmentId = int;



void environment_init(EnvironmentId type);



} // namespace skyland



namespace skyland::weather
{



class Environment
{
public:
    Environment();


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

    virtual void update(Time delta) = 0;
    virtual void rewind(Time delta) = 0;

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


    bool is_night() const;


    virtual EnvironmentId id() const = 0;


    virtual void on_pause();


    virtual bool is_overcast() const
    {
        return false;
    }


    virtual Platform::Screen::Shader shader() const
    {
        return passthrough_shader;
    }


    Conf::String read_conf(const char* field) const;
    Conf::String music() const;
    Conf::String ambiance() const;
};



class CleanEnvironment : public Environment
{
public:
    void update(Time delta) override
    {
        // Nothing...
    }


    void rewind(Time delta) override
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
    ClearSkies()
    {
        timer_ = seconds(rng::choice<7>(rng::utility_state));
    }


    virtual EnvironmentId id() const override;


    void update(Time delta) override;


    void display() override;


    ColorConstant fadein_colorize_tone() const override;


private:
    Time timer_;
};



} // namespace skyland::weather
