#include "fieldHull.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



FieldHull::FieldHull(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void FieldHull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void FieldHull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::field_hull;
}



void FieldHull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::field_hull;
}



} // namespace skyland
