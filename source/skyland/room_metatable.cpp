#include "room_metatable.hpp"


#include "skyland/rooms/core.hpp"
#include "skyland/rooms/exteriorWall.hpp"
#include "skyland/rooms/stairwell.hpp"


namespace skyland {


RoomMetatable<Core, Stairwell, ExteriorWall> __room_metatable;


std::pair<RoomMeta*, int> room_metatable()
{
    return {__room_metatable.table_, __room_metatable.size()};
}


} // namespace skyland
