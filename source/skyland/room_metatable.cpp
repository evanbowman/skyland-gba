#include "room_metatable.hpp"


#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/exteriorWall.hpp"
#include "skyland/rooms/hull.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/workshop.hpp"



namespace skyland {



RoomMetatable<Hull, Cannon, MissileSilo, Stairwell, Workshop, Core>
    __room_metatable;



std::pair<RoomMeta*, int> room_metatable()
{
    return {__room_metatable.table_, __room_metatable.size()};
}



RoomMeta* load_metaclass(const char* name)
{
    auto [mt, ms] = room_metatable();

    for (int i = 0; i < ms; ++i) {
        if (str_cmp(mt[i]->name(), name) == 0) {
            return &mt[i];
        }
    }

    return nullptr;
}



} // namespace skyland
