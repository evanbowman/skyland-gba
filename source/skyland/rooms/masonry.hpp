#pragma once


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Masonry : public Decoration
{
public:
    Masonry(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_masonry)->c_str();
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::masonry;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::masonry;
    }


    static u32 properties()
    {
        return Decoration::properties() & ~RoomProperties::disallow_chimney;
    }


    static const char* name()
    {
        return "masonry";
    }


    static SystemString ui_name()
    {
        return SystemString::block_masonry;
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



} // namespace skyland
