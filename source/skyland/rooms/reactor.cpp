#include "reactor.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Reactor::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_reactor)->c_str();
}



Reactor::Reactor(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Reactor::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Reactor::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() == 0) {
        core_explosion(pfrm, app, center());
    }
}



void Reactor::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::reactor_1;
    buffer[position().x + 1][position().y] = InteriorTile::reactor_2;

    buffer[position().x][position().y + 1] = InteriorTile::reactor_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::reactor_4;

    buffer[position().x][position().y + 2] = InteriorTile::core_2;
    buffer[position().x + 1][position().y + 2] = InteriorTile::plain_floor;
}



void Reactor::render_exterior(App& app, u8 buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_plain_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_plain_2;
}



} // namespace skyland
