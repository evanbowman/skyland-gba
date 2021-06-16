#include "room_metatable.hpp"


#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/exteriorWall.hpp"
#include "skyland/rooms/hull.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/rooms/missileSilo.hpp"


namespace skyland {


RoomMetatable<Hull, Cannon, MissileSilo, Stairwell, Workshop, Core> __room_metatable;


std::pair<RoomMeta*, int> room_metatable()
{
    return {__room_metatable.table_, __room_metatable.size()};
}


} // namespace skyland
