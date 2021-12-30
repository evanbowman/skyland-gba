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



void Forcefield::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::forcefield);
}



void Forcefield::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::forcefield);
}



} // namespace skyland
