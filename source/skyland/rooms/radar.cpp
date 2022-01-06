#include "radar.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Radar::Radar(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Radar::render_interior(App& app, u8 buffer[16][16])
{

    buffer[position().x][position().y] = InteriorTile::radar_1;
    buffer[position().x][position().y + 1] = InteriorTile::radar_2;
}



void Radar::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::radar_1;
    buffer[position().x][position().y + 1] = Tile::radar_2;
}



} // namespace skyland
