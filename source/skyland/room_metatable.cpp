#include "room_metatable.hpp"


#include "skyland/rooms/arcGun.hpp"
#include "skyland/rooms/bridge.hpp"
#include "skyland/rooms/bronzeHull.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/manufactory.hpp"
#include "skyland/rooms/hull.hpp"
#include "skyland/rooms/infirmary.hpp"
#include "skyland/rooms/ionCannon.hpp"
#include "skyland/rooms/ionFizzler.hpp"
#include "skyland/rooms/masonry.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/nemesis.hpp"
#include "skyland/rooms/palm.hpp"
#include "skyland/rooms/plunderedRoom.hpp"
#include "skyland/rooms/poweredHull.hpp"
#include "skyland/rooms/radar.hpp"
#include "skyland/rooms/reactor.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/rooms/shrubbery.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/statue.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/rooms/workshop.hpp"



namespace skyland {



template <int plugin_slots, typename... Rooms> struct RoomMetatable {
public:
    template <size_t i, typename First, typename... Rest> void init()
    {
        table_[i].template init<First>();

        if constexpr (sizeof...(Rest) > 0) {
            init<i + 1, Rest...>();
        }
    }

    RoomMetatable()
    {
        init<0, Rooms...>();

        for (int i = 0; i < plugin_slots; ++i) {
            table_[sizeof...(Rooms) + i].init_plugin();
        }

        for (MetaclassIndex i = 0; i < plugin_rooms_begin(); ++i) {
            if (not(table_[i]->properties() &
                    RoomProperties::locked_by_default)) {
                enabled_rooms_.set(i, true);
            }
        }
    }

    int size()
    {
        return sizeof...(Rooms) + plugin_slots;
    }

    // Returns the number of builtin rooms. Relevant because we may want to
    // disable plugin (dlc) rooms for certain game modes.
    static constexpr int builtin_slots_end()
    {
        return sizeof...(Rooms);
    }

    RoomMeta table_[sizeof...(Rooms) + plugin_slots];
    Bitvector<sizeof...(Rooms) + plugin_slots> enabled_rooms_;
};



using RoomMetatableType = RoomMetatable<15,
                                        // walls
                                        Hull,
                                        BronzeHull,
                                        Forcefield,
                                        PoweredHull,
                                        IonFizzler,
                                        // weapons
                                        Cannon,
                                        IonCannon,
                                        FlakGun,
                                        ArcGun,
                                        Nemesis,
                                        Decimator,
                                        MissileSilo,
                                        // factories
                                        Workshop,
                                        Manufactory,
                                        // power generation
                                        Core,
                                        Reactor,
                                        // misc
                                        Stairwell,
                                        Bulkhead,
                                        Infirmary,
                                        CargoBay,
                                        Radar,
                                        Transporter,
                                        Replicator,
                                        DroneBay,
                                        // decoration
                                        Statue,
                                        Bridge,
                                        Palm,
                                        Shrubbery,
                                        Masonry,
                                        PlunderedRoom>;



static auto& __metatable()
{
    static RoomMetatableType __room_metatable;

    return __room_metatable;
}



bool is_enabled(MetaclassIndex index)
{
    return __metatable().enabled_rooms_.get(index);
}



void set_enabled(MetaclassIndex index, bool enabled)
{
    if (index >= plugin_rooms_begin()) {
        Platform::fatal("Attempt to manually set enabled bit for plugin room!");
    }

    __metatable().enabled_rooms_.set(index, enabled);
}



void plugin_rooms_unregister()
{
    for (int i = plugin_rooms_begin(); i < __metatable().size(); ++i) {
        __metatable().enabled_rooms_.set(i, false);

        if (auto b = dynamic_cast<RoomMeta::PluginBox*>(
                __metatable().table_[i].box())) {
            b->info_.reset();
        } else {
            Platform::fatal(
                "Metaclass Boxed in plugin sector is not a PluginBox?");
        }
    }
}



bool plugin_room_register(lisp::Value* config)
{
    for (int i = plugin_rooms_begin(); i < __metatable().size(); ++i) {
        if (not __metatable().enabled_rooms_.get(i)) {
            // We've found an unused slot, where we can register a new plugin
            // room.

            // Lock the slot.
            __metatable().enabled_rooms_.set(i, true);

            if (auto b = dynamic_cast<RoomMeta::PluginBox*>(
                    __metatable().table_[i].box())) {
                b->info_ = config;
            } else {
                Platform::fatal("program logic error: metaclass"
                                "in plugin sector is not a plugin.");
            }

            return true;
        }
    }
    return false;
}



MetaclassIndex plugin_rooms_begin()
{
    return RoomMetatableType::builtin_slots_end();
}



const RoomMeta* forcefield_mt = load_metaclass(Forcefield::name());
const RoomMeta* cannon_mt = load_metaclass(Cannon::name());
const RoomMeta* missile_silo_mt = load_metaclass(MissileSilo::name());
const RoomMeta* ion_cannon_mt = load_metaclass(IonCannon::name());
const RoomMeta* bulkhead_mt = load_metaclass(Bulkhead::name());
const RoomMeta* drone_bay_mt = load_metaclass(DroneBay::name());



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
