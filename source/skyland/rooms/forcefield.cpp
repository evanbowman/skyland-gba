#include "forcefield.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Forcefield::Forcefield(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Forcefield::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Forcefield::render_interior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::forcefield;
}



void Forcefield::render_exterior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::forcefield;
}



} // namespace skyland
