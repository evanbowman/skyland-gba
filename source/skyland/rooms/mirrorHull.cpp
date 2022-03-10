#include "mirrorHull.hpp"



namespace skyland {



MirrorHull::MirrorHull(Island* parent, const Vec2<u8>& position)
    : Hull(parent, position, name())
{
}



void MirrorHull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::mirror_hull;
}



void MirrorHull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::mirror_hull;
}



} // namespace skyland
