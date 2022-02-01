#include "bronzeHull.hpp"



namespace skyland {



BronzeHull::BronzeHull(Island* parent, const Vec2<u8>& position) :
    Hull(parent, position, name())
{
}



void BronzeHull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::bronze;
}



void BronzeHull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::bronze;
}



}
