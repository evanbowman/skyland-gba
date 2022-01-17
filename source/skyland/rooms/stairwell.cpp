#include "stairwell.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"


namespace skyland {


void Stairwell::format_description(StringBuffer<512>& buffer)
{
    buffer +=
        "A structure designed to bridge a large vertical distance "
        "by dividing it into smaller vertical distances, often called steps.";
}


Stairwell::Stairwell(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}


void Stairwell::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}


void Stairwell::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid_2;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_base;
}


void Stairwell::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 3] = Tile::wall_window_2;
}



void Stairwell::plot_walkable_zones(App& app, bool matrix[16][16])
{
    // All tiles in a stairwell are walkable, that's kind of the point.
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



} // namespace skyland
