#include "foundry.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void Foundry::format_description(StringBuffer<512>& buffer)
{
    buffer += "An upgraded workshop. "
              "Slightly lowers the construction cost of all rooms. "
              "Requires a workshop.";
}



Foundry::Foundry(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}



void Foundry::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Foundry::render_interior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::foundry_1;
    buffer[x + 1][y] = InteriorTile::foundry_2;
    buffer[x + 2][y] = InteriorTile::foundry_3;

    buffer[x][y + 1] = InteriorTile::foundry_4;
    buffer[x + 1][y + 1] = InteriorTile::foundry_5;
    buffer[x + 2][y + 1] = InteriorTile::plain_floor;
}



void Foundry::render_exterior(App& app, u8 buffer[16][16])
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
