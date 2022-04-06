#pragma once

#include "number/numeric.hpp"



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
};



class ClearEnvironment : public Environment
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



}
