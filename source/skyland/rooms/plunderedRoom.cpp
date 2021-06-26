#include "plunderedRoom.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



PlunderedRoom::PlunderedRoom(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position, Health(20))
{
}


void PlunderedRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void PlunderedRoom::render_interior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(
        layer, position().x, position().y, InteriorTile::plundered_room);
    pfrm.set_tile(
        layer, position().x, position().y + 1, InteriorTile::plundered_room);
}



void PlunderedRoom::render_exterior(Platform& pfrm, Layer layer)
{
    pfrm.set_tile(layer, position().x, position().y, Tile::plundered_room);
    pfrm.set_tile(layer, position().x, position().y + 1, Tile::plundered_room);
}



} // namespace skyland
