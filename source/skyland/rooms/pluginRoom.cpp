#include "pluginRoom.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



PluginRoom::PluginRoom(Island* parent,
                       const Vec2<u8>& position,
                       RoomMeta* metaclass)
    : Room(parent, (*metaclass)->name(), position)
{
}



void PluginRoom::update(Platform&, App&, Microseconds delta)
{
    // ...
}



} // namespace skyland
