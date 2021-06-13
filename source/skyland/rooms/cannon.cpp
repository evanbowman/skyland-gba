#include "cannon.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Cannon::Cannon(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(200))
{
}



void Cannon::update(Platform&, App&, Microseconds delta)
{
}



void Cannon::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::cannon_1);
}



void Cannon::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::cannon_1);
}



} // namespace skyland
