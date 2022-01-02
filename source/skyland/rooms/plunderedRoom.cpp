#include "plunderedRoom.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



PlunderedRoom::PlunderedRoom(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}


void PlunderedRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void PlunderedRoom::render_interior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::plundered_room;
    buffer[position().x][position().y + 1] = InteriorTile::plundered_room;
}



void PlunderedRoom::render_exterior(u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::plundered_room;
    buffer[position().x][position().y + 1] = Tile::plundered_room;
}



} // namespace skyland
