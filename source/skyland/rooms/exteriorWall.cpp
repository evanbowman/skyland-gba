#include "exteriorWall.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



ExteriorWall::ExteriorWall(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(100))
{
}



void ExteriorWall::update(Platform&, App&, Microseconds delta)
{
    // ...
}



void ExteriorWall::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::empty);
    pfrm.set_tile(layer, position().x, position().y + 1, InteriorTile::empty);
}



void ExteriorWall::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::armored_wall_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::armored_wall_2);
}



} // namespace skyland
