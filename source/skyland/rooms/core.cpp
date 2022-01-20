#include "core.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"


namespace skyland {



void Core::format_description(StringBuffer<512>& buffer)
{
    buffer += "Every flying island needs a power core to stay afloat! "
        "Supplies power to your castle's structures. You must have a "
        "workshop to build power cores.";
}



Core::Core(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}


void Core::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}


void Core::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::core_1;
    buffer[position().x][position().y + 1] = InteriorTile::core_2;
    buffer[position().x + 1][position().y] = InteriorTile::core_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::core_4;
}


void Core::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}


} // namespace skyland
