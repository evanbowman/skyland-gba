#include "room_metatable.hpp"


#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/hull.hpp"
#include "skyland/rooms/infirmary.hpp"
#include "skyland/rooms/ionCannon.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/plunderedRoom.hpp"
#include "skyland/rooms/radar.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/rooms/workshop.hpp"
#include "skyland/rooms/infestor.hpp"



namespace skyland {



static auto& __metatable()
{
    static RoomMetatable<Hull,
                         Forcefield,
                         Cannon,
                         IonCannon,
                         FlakGun,
                         MissileSilo,
                         Infestor,
                         Stairwell,
                         Bulkhead,
                         Workshop,
                         Infirmary,
                         Core,
                         Radar,
                         PlunderedRoom,
                         Transporter,
                         Replicator>
        __room_metatable;

    return __room_metatable;
}



const RoomMeta* forcefield_mt = load_metaclass(Forcefield::name());
const RoomMeta* cannon_mt = load_metaclass(Cannon::name());
const RoomMeta* missile_silo_mt = load_metaclass(MissileSilo::name());
const RoomMeta* ion_cannon_mt = load_metaclass(IonCannon::name());
const RoomMeta* bulkhead_mt = load_metaclass(Bulkhead::name());



std::pair<RoomMeta*, int> room_metatable()
{
    return {__metatable().table_, __metatable().size()};
}



MetaclassIndex metaclass_index(const char* name)
{
    auto [mt, ms] = room_metatable();

    for (int i = 0; i < ms; ++i) {
        if (str_cmp(mt[i]->name(), name) == 0) {
            return i;
        }
    }

    return 0;
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



RoomMeta* load_metaclass(MetaclassIndex index)
{
    auto [mt, ms] = room_metatable();

    if (ms > index) {
        return &mt[index];
    }

    return nullptr;
}



} // namespace skyland
