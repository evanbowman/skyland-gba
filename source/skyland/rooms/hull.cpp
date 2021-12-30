#include "hull.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Hull::Hull(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Hull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Hull::render_interior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::hull;
}



void Hull::render_exterior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::hull;
}



} // namespace skyland
