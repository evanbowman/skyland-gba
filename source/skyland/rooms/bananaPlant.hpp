#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland {



class BananaPlant : public Decoration
{
public:
    BananaPlant(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_banana_plant)->c_str();
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::banana_plant;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::banana_plant;
    }


    static u32 properties()
    {
        return Decoration::properties();
    }


    static const char* name()
    {
        return "banana-plant";
    }


    static SystemString ui_name()
    {
        return SystemString::block_banana_plant;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1864;
    }


    static Icon unsel_icon()
    {
        return 1880;
    }
};



} // namespace skyland
