#pragma once


#include "decoration.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class Masonry : public Decoration {
public:

    Masonry(Island* parent, const Vec2<u8>& position) :
        Decoration(parent, name(), size(), position)
    {
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += "While ineffective against most modern weaponry, "
            "villagers in skyland still build structures with stone, because "
            "it looks nice.";
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::masonry;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::masonry;
    }


    bool disallow_chimney() override
    {
        return false;
    }


    static const char* name()
    {
        return "masonry";
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1448;
    }


    static Icon unsel_icon()
    {
        return 1464;
    }

};



}
