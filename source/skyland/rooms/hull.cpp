#include "hull.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void Hull::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_hull)->c_str();
}



Hull::Hull(Island* parent, const Vec2<u8>& position, const char* n)
    : Room(parent, n, position)
{
}



void Hull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Hull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::hull;
}



void Hull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::hull;
}



} // namespace skyland
