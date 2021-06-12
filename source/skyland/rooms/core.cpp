#include "core.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"




namespace skyland {



Core::Core(Island* parent, const Vec2<u8>& position) :
    Room(parent, {2, 2}, position)
{
}



void Core::update(Platform&, App&, Microseconds delta)
{
    // ...
}



void Core::render_interior(Platform& pfrm, Layer layer)
{

}



void Core::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_window_2);
    pfrm.set_tile(layer, position().x + 1, position().y, Tile::wall_plain_1);
    pfrm.set_tile(layer, position().x + 1, position().y + 1, Tile::wall_plain_2);
}



}
