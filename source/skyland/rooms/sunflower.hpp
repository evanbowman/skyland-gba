#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Sunflower : public Decoration
{
public:
    Sunflower(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::sunflower;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::sunflower;
    }


    static u32 properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::highly_flammable;
    }


    static const char* name()
    {
        return "sunflower";
    }


    static SystemString ui_name()
    {
        return SystemString::block_sunflower;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1896;
    }


    static Icon unsel_icon()
    {
        return 1912;
    }
};



} // namespace skyland
