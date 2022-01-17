#include "plunderedRoom.hpp"
#include "platform/platform.hpp"
#include "skyland/tile.hpp"



namespace skyland {



void PlunderedRoom::format_description(StringBuffer<512>& buffer)
{
    buffer += "A placeholder structure created after a character destroys a "
              "room by plundering.";
}



PlunderedRoom::PlunderedRoom(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), size(), position)
{
}


void PlunderedRoom::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void PlunderedRoom::render_interior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::plundered_room;
    buffer[position().x][position().y + 1] = InteriorTile::plundered_room;
}



void PlunderedRoom::render_exterior(App& app, u8 buffer[16][16])
{
    buffer[position().x][position().y] = Tile::plundered_room;
    buffer[position().x][position().y + 1] = Tile::plundered_room;
}



} // namespace skyland
