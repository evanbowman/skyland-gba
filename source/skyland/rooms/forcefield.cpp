#include "forcefield.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void Forcefield::format_description(StringBuffer<512>& buffer)
{
    buffer += "Blocks enemy projectiles, but allows your own weapons to shoot "
              "through. Vulnerable to ion damage.";
}



Forcefield::Forcefield(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Forcefield::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Forcefield::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::forcefield;
}



void Forcefield::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::forcefield;
}



} // namespace skyland
