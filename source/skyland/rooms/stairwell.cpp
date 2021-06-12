#include "stairwell.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



Stairwell::Stairwell(Island* parent, const Vec2<u8>& position) :
    Room(parent, {1, 4}, position)
{
}



void Stairwell::update(Platform&, App&, Microseconds delta)
{

}



void Stairwell::render_interior(Platform& pfrm, Layer layer)
{

}



void Stairwell::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::wall_window_1);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::wall_window_middle_2);
    pfrm.set_tile(layer, position().x, position().y + 2, Tile::wall_window_middle_1);
    pfrm.set_tile(layer, position().x, position().y + 3, Tile::wall_window_2);
}



}
