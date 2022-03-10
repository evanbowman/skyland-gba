#include "stackedHull.hpp"



namespace skyland {



StackedHull::StackedHull(Island* parent, const Vec2<u8>& position)
    : Hull(parent, position, name())
{
}



void StackedHull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::stacked_hull;
}



void StackedHull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::stacked_hull;
}



} // namespace skyland
