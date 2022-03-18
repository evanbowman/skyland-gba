#pragma once

#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Statue : public Decoration
{
public:
    Statue(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::statue_1;
        buffer[position().x][position().y + 1] = InteriorTile::statue_2;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::statue_1;
        buffer[position().x][position().y + 1] = Tile::statue_2;
    }


    static const char* name()
    {
        return "statue";
    }


    static SystemString ui_name()
    {
        return SystemString::block_statue;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static Icon icon()
    {
        return 1480;
    }


    static Icon unsel_icon()
    {
        return 1496;
    }
};



} // namespace skyland
