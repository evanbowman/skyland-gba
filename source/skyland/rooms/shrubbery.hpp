#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Shrubbery : public Decoration
{
public:
    Shrubbery(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_shrubbery)->c_str();
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    static u32 properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::highly_flammable;
    }


    static const char* name()
    {
        return "shrubbery";
    }


    static SystemString ui_name()
    {
        return SystemString::block_shrubbery;
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



} // namespace skyland
