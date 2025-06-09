////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "room_metatable.hpp"

#include "script/lisp.hpp"

#include "ext_workram_data.hpp"
#include "roomPluginInfo.hpp"
#include "skyland/rooms/amplifier.hpp"
#include "skyland/rooms/annihilator.hpp"
#include "skyland/rooms/arcGun.hpp"
#include "skyland/rooms/ballista.hpp"
#include "skyland/rooms/balloon.hpp"
#include "skyland/rooms/bananaPlant.hpp"
#include "skyland/rooms/barrier.hpp"
#include "skyland/rooms/basalt.hpp"
#include "skyland/rooms/beamGun.hpp"
#include "skyland/rooms/bell.hpp"
#include "skyland/rooms/bridge.hpp"
#include "skyland/rooms/bronzeHull.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/canvas.hpp"
#include "skyland/rooms/cargoBay.hpp"
#include "skyland/rooms/chaosCore.hpp"
#include "skyland/rooms/cloak.hpp"
#include "skyland/rooms/clumpBomb.hpp"
#include "skyland/rooms/commandModule.hpp"
#include "skyland/rooms/core.hpp"
#include "skyland/rooms/crane.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/deflector.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/escapeBeacon.hpp"
#include "skyland/rooms/fireCharge.hpp"
#include "skyland/rooms/flakGun.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/fountain.hpp"
#include "skyland/rooms/gold.hpp"
#include "skyland/rooms/hull.hpp"
#include "skyland/rooms/ice.hpp"
#include "skyland/rooms/incinerator.hpp"
#include "skyland/rooms/infirmary.hpp"
#include "skyland/rooms/ionCannon.hpp"
#include "skyland/rooms/ionFizzler.hpp"
#include "skyland/rooms/ladder.hpp"
#include "skyland/rooms/ladyLiberty.hpp"
#include "skyland/rooms/lava.hpp"
#include "skyland/rooms/lemonTree.hpp"
#include "skyland/rooms/manufactory.hpp"
#include "skyland/rooms/marketStall.hpp"
#include "skyland/rooms/masonry.hpp"
#include "skyland/rooms/mirrorHull.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/mycelium.hpp"
#include "skyland/rooms/nemesis.hpp"
#include "skyland/rooms/palm.hpp"
#include "skyland/rooms/phaseShifter.hpp"
#include "skyland/rooms/piston.hpp"
#include "skyland/rooms/plunderedRoom.hpp"
#include "skyland/rooms/portal.hpp"
#include "skyland/rooms/poweredHull.hpp"
#include "skyland/rooms/pummeler.hpp"
#include "skyland/rooms/qrBlock.hpp"
#include "skyland/rooms/radar.hpp"
#include "skyland/rooms/radiator.hpp"
#include "skyland/rooms/reactor.hpp"
#include "skyland/rooms/replicator.hpp"
#include "skyland/rooms/rocketSilo.hpp"
#include "skyland/rooms/shrubbery.hpp"
#include "skyland/rooms/snow.hpp"
#include "skyland/rooms/solarCell.hpp"
#include "skyland/rooms/sparkCannon.hpp"
#include "skyland/rooms/speaker.hpp"
#include "skyland/rooms/stackedHull.hpp"
#include "skyland/rooms/stairwell.hpp"
#include "skyland/rooms/statue.hpp"
#include "skyland/rooms/sunflower.hpp"
#include "skyland/rooms/swerveMissileSilo.hpp"
#include "skyland/rooms/sylphCannon.hpp"
#include "skyland/rooms/synth.hpp"
#include "skyland/rooms/targetingComputer.hpp"
#include "skyland/rooms/tnt.hpp"
#include "skyland/rooms/torch.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/rooms/warEngine.hpp"
#include "skyland/rooms/warhead.hpp"
#include "skyland/rooms/water.hpp"
#include "skyland/rooms/weatherEngine.hpp"
#include "skyland/rooms/windmill.hpp"
#include "skyland/rooms/workshop.hpp"



namespace skyland
{



template <typename T> struct InfoImpl : public RoomMeta::Info
{
    InfoImpl()
        // NOTE: the game will fill in these parameters from configuration
        // later on.
        : health_(T::default_health()), cost_(T::default_cost()),
          power_(T::default_power())
    {
    }

    void
    construct(void* address, Island* parent, const RoomCoord& position) override
    {
        static_assert(sizeof(T) <= room_pool::max_room_size);
        static_assert(alignof(T) <= room_pool::alignment);

        new (address) T(parent, position);
    }

    void create(Island* parent,
                const RoomCoord& position,
                bool do_repaint) const override
    {
        parent->add_room<T>(position, do_repaint);
    }

    // RoomPtr<Room> create(Island* parent,
    //                      const RoomCoord& position) const override
    // {
    //     return room_pool::alloc<T>(parent, position);
    // }

    const char* name() const override
    {
        return T::name();
    }

    SystemStringBuffer ui_name() const override
    {
        return loadstr(T::ui_name());
    }

    Room::Icon icon() const override
    {
        return T::icon();
    }

    Room::Icon unsel_icon() const override
    {
        return T::unsel_icon();
    }

    Vec2<u8> size() const override
    {
        return T::size();
    }

    Coins cost() const override
    {
        return cost_;
    }

    ATP atp_value() const override
    {
        return T::atp_value();
    }

    Power consumes_power() const override
    {
        return power_;
    }

    RoomProperties::Bitmask properties() const override
    {
        return T::properties();
    }

    Room::Category category() const override
    {
        return T::category(); // TODO...
    }


    Room::WeaponOrientation weapon_orientation() const override
    {
        auto o = T::weapon_orientation();
        if (category() == Room::Category::weapon and
            o == Room::WeaponOrientation::none) {
            Platform::fatal(format("weapon orientation config missing "
                                   "for $",
                                   T::name()));
        }
        return o;
    }


    void format_description(StringBuffer<512>& buffer) const override
    {
        return T::format_description(buffer);
    }

    Health full_health() const override
    {
        return health_;
    }

    void configure(Health health, Coins cost, Power power) override
    {
        health_ = health;
        cost_ = cost;
        power_ = power;
    }

    s16 health_;
    s16 cost_;
    s16 power_;
};



template <typename T> void RoomMeta::init()
{
    static_assert(sizeof buffer_ >= sizeof(InfoImpl<T>));
    static_assert(align >= alignof(InfoImpl<T>));

    new (buffer_) InfoImpl<T>();
}



void RoomMeta::init_plugin()
{
    static_assert(sizeof buffer_ >= sizeof(RoomPluginInfo));
    static_assert(align >= alignof(RoomPluginInfo));

    new (buffer_) RoomPluginInfo(this);
}



template <int plugin_slots, typename... Rooms> struct RoomMetatable
{
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



using RoomMetatableType = RoomMetatable<11,
                                        // walls
                                        Hull,
                                        BronzeHull,
                                        Forcefield,
                                        Forcefield2,
                                        PoweredHull,
                                        IonFizzler,
                                        Radiator,
                                        Cloak,
                                        MirrorHull,
                                        StackedHull,
                                        Mycelium,
                                        Barrier,
                                        // weapons
                                        Cannon,
                                        IonCannon,
                                        FlakGun,
                                        ArcGun,
                                        Nemesis,
                                        FireCharge,
                                        SylphCannon,
                                        Decimator,
                                        Annihilator,
                                        SparkCannon,
                                        Incinerator,
                                        BeamGun,
                                        Ballista,
                                        MissileSilo,
                                        RocketSilo,
                                        ClumpBomb,
                                        Warhead,
                                        // factories
                                        Workshop,
                                        Manufactory,
                                        // power generation
                                        Core,
                                        Reactor,
                                        SolarCell,
                                        BackupCore,
                                        WarEngine,
                                        ChaosCore,
                                        Windmill,
                                        Balloon,
                                        // misc
                                        Stairwell,
                                        Ladder,
                                        LadderPlus,
                                        StairwellPlus,
                                        StairwellPlusPlus,
                                        Portal,
                                        Bulkhead,
                                        Infirmary,
                                        CargoBay,
                                        Crane,
                                        WeatherEngine,
                                        Water,
                                        WaterSource,
                                        Ice,
                                        Explosive,
                                        TNT,
                                        Radar,
                                        Transporter,
                                        TargetingComputer,
                                        EscapeBeacon,
                                        Replicator,
                                        DroneBay,
                                        Deflector,
                                        Amplifier,
                                        PhaseShifter,
                                        // decoration
                                        Bell,
                                        Speaker,
                                        Synth,
                                        Statue,
                                        LadyLiberty,
                                        Bridge,
                                        Fountain,
                                        Torch,
                                        Palm,
                                        LemonTree,
                                        Sunflower,
                                        Shrubbery,
                                        BananaPlant,
                                        Masonry,
                                        QrBlock,
                                        Basalt,
                                        Snow,
                                        MarketStall,
                                        PlunderedRoom,
                                        Canvas>;



static EXT_WORKRAM_DATA RoomMetatableType __room_metatable;



static auto& __metatable()
{
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

        // FIXME: do checked cast. RTTI unsupported on some embedded platforms,
        // due to no libstdc++. This code doesn't run anyway.
        if (auto b = reinterpret_cast<RoomPluginInfo*>(
                __metatable().table_[i].box())) {
            b->info_.reset();
        } else {
            Platform::fatal(
                "Metaclass Boxed in plugin sector is not a PluginInfo?");
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

            if (auto b = reinterpret_cast<RoomPluginInfo*>(
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
const RoomMeta* forcefield2_mt = load_metaclass(Forcefield2::name());
const RoomMeta* cannon_mt = load_metaclass(Cannon::name());
const RoomMeta* missile_silo_mt = load_metaclass(MissileSilo::name());
const RoomMeta* ion_cannon_mt = load_metaclass(IonCannon::name());
const RoomMeta* bulkhead_mt = load_metaclass(Bulkhead::name());
const RoomMeta* drone_bay_mt = load_metaclass(DroneBay::name());
#define G_MT const RoomMeta*
EXT_WORKRAM_DATA G_MT decimator_mt = load_metaclass(Decimator::name());
EXT_WORKRAM_DATA G_MT flak_gun_mt = load_metaclass(FlakGun::name());
EXT_WORKRAM_DATA G_MT radiator_mt = load_metaclass(Radiator::name());
EXT_WORKRAM_DATA G_MT infirmary_mt = load_metaclass(Infirmary::name());
EXT_WORKRAM_DATA G_MT transporter_mt = load_metaclass(Transporter::name());
EXT_WORKRAM_DATA G_MT chaos_core_mt = load_metaclass(ChaosCore::name());



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



RoomMeta& require_metaclass(const char* name)
{
    if (auto mt = load_metaclass(name)) {
        return *mt;
    }

    Platform::fatal(format("missing class %", name).c_str());
}



RoomMeta* load_metaclass(const char* name)
{
    auto [mt, ms] = room_metatable();

    for (int i = 0; i < __room_metatable.builtin_slots_end(); ++i) {
        if (str_cmp(mt[i]->name(), name) == 0) {
            return &mt[i];
        }
    }

    const char* removed_blocks_from_old_versions[] = {
        "gold", "command-module", "lava", "lava-source", "cesium"};

    for (auto type : removed_blocks_from_old_versions) {
        if (str_eq(type, name)) {
            // If a block was deprecated and later removed from the game, load
            // it as a hull block (better than raising a fatal error...)
            return load_metaclass("hull");
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



static Bitvector<128> hidden_rooms;



void room_set_hidden(MetaclassIndex idx, bool hidden)
{
    hidden_rooms.set(idx, hidden);
}



bool room_hidden(MetaclassIndex idx)
{
    return hidden_rooms.get(idx);
}



} // namespace skyland
