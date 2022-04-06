#pragma once

#include "number/numeric.hpp"
#include "graphics/color.hpp"
#include "platform/platform.hpp"



class Platform;



namespace skyland
{



class App;



class Environment
{
public:


    virtual ~Environment()
    {
    }


    virtual void update(Platform& pfrm, App& app, Microseconds delta) = 0;


    virtual const char* player_island_texture() const = 0;
    virtual const char* player_island_interior_texture() const = 0;

    virtual const char* opponent_island_texture() const = 0;
    virtual const char* opponent_island_interior_texture() const = 0;

    virtual const char* sprite_texture() const = 0;
    virtual const char* background_texture() const = 0;


    virtual ColorConstant* player_island_palette_override() const
    {
        return nullptr;
    }

    virtual ColorConstant* player_island_interior_palette_override() const
    {
        return nullptr;
    }

    virtual ColorConstant* opponent_island_palette_override() const
    {
        return nullptr;
    }

    virtual ColorConstant* opponent_island_interior_palette_override() const
    {
        return nullptr;
    }

    virtual ColorConstant* sprite_palette_override() const
    {
        return nullptr;
    }

    virtual ColorConstant* background_palette_override() const
    {
        return nullptr;
    }

    virtual Platform::Screen::Shader shader() const
    {
        return passthrough_shader;
    }
};



class CleanEnvironment : public Environment
{
public:


    void update(Platform& pfrm, App& app, Microseconds delta)
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



class ClearEnvironment : public CleanEnvironment
{
public:

    // ...

};



}
