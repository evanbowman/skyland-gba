#pragma once


#include "decoration.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class Shrubbery : public Decoration {
public:

    Shrubbery(Island* parent, const Vec2<u8>& position) :
        Decoration(parent, name(), size(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += "\"Ni!\"";
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    static const char* name()
    {
        return "shrubbery";
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1416;
    }


    static Icon unsel_icon()
    {
        return 1432;
    }

};



}
