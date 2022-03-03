#pragma once

#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class Palm : public Decoration
{
public:
    Palm(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::palm_1;
        buffer[position().x][position().y + 1] = Tile::palm_2;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::palm_1;
        buffer[position().x][position().y + 1] = Tile::palm_2;
    }


    static const char* name()
    {
        return "coconut-palm";
    }


    static SystemString ui_name()
    {
        return SystemString::block_palm;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static Icon icon()
    {
        return 1384;
    }


    static Icon unsel_icon()
    {
        return 1400;
    }
};



} // namespace skyland
