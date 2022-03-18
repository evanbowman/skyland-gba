#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class LadyLiberty : public Decoration
{
public:
    LadyLiberty(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_lady_liberty)->c_str();
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::liberty_1;
        buffer[position().x + 1][position().y] = InteriorTile::liberty_2;
        buffer[position().x][position().y + 1] = InteriorTile::liberty_3;
        buffer[position().x + 1][position().y + 1] = InteriorTile::liberty_4;
        buffer[position().x][position().y + 2] = InteriorTile::liberty_5;
        buffer[position().x + 1][position().y + 2] = InteriorTile::liberty_6;
        buffer[position().x][position().y + 3] = InteriorTile::liberty_7;
        buffer[position().x + 1][position().y + 3] = InteriorTile::liberty_8;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::liberty_1;
        buffer[position().x + 1][position().y] = Tile::liberty_2;
        buffer[position().x][position().y + 1] = Tile::liberty_3;
        buffer[position().x + 1][position().y + 1] = Tile::liberty_4;
        buffer[position().x][position().y + 2] = Tile::liberty_5;
        buffer[position().x + 1][position().y + 2] = Tile::liberty_6;
        buffer[position().x][position().y + 3] = Tile::liberty_7;
        buffer[position().x + 1][position().y + 3] = Tile::liberty_8;
    }


    static u32 properties()
    {
        return Decoration::properties() & ~RoomProperties::locked_by_default;
    }


    static const char* name()
    {
        return "lady-liberty";
    }


    static SystemString ui_name()
    {
        return SystemString::block_lady_liberty;
    }


    static Vec2<u8> size()
    {
        return {2, 4};
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



} // namespace skyland
