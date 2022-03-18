#include "workshop.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Workshop::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_workshop)->c_str();
}



Workshop::Workshop(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Workshop::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Workshop::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::workshop_1;
    buffer[position().x][position().y + 1] = InteriorTile::workshop_2;
    buffer[position().x + 1][position().y] = InteriorTile::workshop_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::workshop_4;
}



void Workshop::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



} // namespace skyland
