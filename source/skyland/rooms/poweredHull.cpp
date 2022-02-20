#include "poweredHull.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void PoweredHull::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_energized_hull)->c_str();
}



PoweredHull::PoweredHull(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void PoweredHull::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void PoweredHull::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::field_hull;
}



void PoweredHull::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::field_hull;
}



} // namespace skyland
