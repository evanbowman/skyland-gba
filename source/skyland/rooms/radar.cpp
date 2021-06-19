#include "radar.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Radar::Radar(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(30))
{
}



void Radar::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, InteriorTile::empty);
    pfrm.set_tile(layer, position().x, position().y + 1, InteriorTile::empty);
}



void Radar::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::armored_wall_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::armored_wall_2);
}



}
