#include "workshop.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Workshop::Workshop(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Workshop::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Workshop::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::workshop_1);
    pfrm.set_tile(
        layer, position().x, position().y + 1, InteriorTile::workshop_2);
    pfrm.set_tile(
        layer, position().x + 1, position().y, InteriorTile::workshop_3);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, InteriorTile::workshop_4);
}



void Workshop::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_window_2);
    pfrm.set_tile(layer, position().x + 1, position().y, Tile::wall_plain_1);
    pfrm.set_tile(
        layer, position().x + 1, position().y + 1, Tile::wall_plain_2);
}



} // namespace skyland
