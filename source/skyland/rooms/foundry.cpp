#include "foundry.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Foundry::Foundry(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Foundry::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Foundry::render_interior(u8 buffer[16][16])
{
    auto pos = position();
    buffer[pos.x + 1][pos.y] = InteriorTile::decimator_1;
    buffer[pos.x + 1][pos.y + 1] = InteriorTile::decimator_2;
    buffer[pos.x][pos.y + 1] = InteriorTile::plain_floor;
    buffer[pos.x][pos.y] = InteriorTile::decimator_int;
}



void Foundry::render_exterior(u8 buffer[16][16])
{
    auto pos = position();
    buffer[pos.x + 1][pos.y] = Tile::decimator_1;
    buffer[pos.x + 1][pos.y + 1] = Tile::decimator_2;
    buffer[pos.x][pos.y] = Tile::armored_wall_1;
    buffer[pos.x][pos.y + 1] = Tile::wall_plain_2;
}



} // namespace skyland
