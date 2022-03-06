#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class Fountain : public Decoration
{
public:
    Fountain(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::fountain;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::fountain;
    }


    static const char* name()
    {
        return "fountain";
    }


    static SystemString ui_name()
    {
        return SystemString::block_fountain;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1928;
    }


    static Icon unsel_icon()
    {
        return 1944;
    }
};



} // namespace skyland
