#include "barrier.hpp"



namespace skyland
{



Barrier::Barrier(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Barrier::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Barrier::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::barrier;
}



void Barrier::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::barrier;
}



void Barrier::apply_damage(Platform& pfrm, App& app, Health damage)
{
    // Takes no damage, kind of the whole point.
    Room::apply_damage(pfrm, app, 0);
}



} // namespace skyland
