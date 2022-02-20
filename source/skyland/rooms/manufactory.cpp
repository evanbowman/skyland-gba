#include "manufactory.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void Manufactory::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_manufactory)->c_str();
}



Manufactory::Manufactory(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Manufactory::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Manufactory::render_interior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::manufactory_1;
    buffer[x + 1][y] = InteriorTile::manufactory_2;
    buffer[x + 2][y] = InteriorTile::manufactory_3;

    buffer[x][y + 1] = InteriorTile::manufactory_4;
    buffer[x + 1][y + 1] = InteriorTile::manufactory_5;
    buffer[x + 2][y + 1] = InteriorTile::plain_floor;
}



void Manufactory::render_exterior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_2;
    buffer[x + 1][y] = Tile::wall_plain_1;
    buffer[x + 1][y + 1] = Tile::wall_plain_2;
    buffer[x + 2][y] = Tile::wall_window_1;
    buffer[x + 2][y + 1] = Tile::wall_window_2;
}



} // namespace skyland
