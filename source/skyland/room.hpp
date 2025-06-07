////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "block.hpp"
#include "coord.hpp"
#include "entity.hpp"
#include "entity/character/character.hpp"
#include "health.hpp"
#include "metaclassIndex.hpp"
#include "number/numeric.hpp"
#include "platform/layer.hpp"
#include "power.hpp"
#include "scene.hpp"
#include "script/value.hpp"
#include "targetQueue.hpp"
#include "tileId.hpp"
#include <limits>



class Platform;



namespace skyland
{



class App;
class Entity;
class Island;
class Character;
class Drone;
struct RoomMeta;
class Weapon;



struct RoomProperties
{
    using Bitmask = u32;

    enum Value : Bitmask {
        none = 0,

        // Workshop required to build this room
        workshop_required = (1 << 0),

        // Room not shown on the construction menu
        not_constructible = (1 << 1),

        // Room is a plugin, i.e. implemented as DLC
        plugin = (1 << 2),

        // Manufactory required to build this room
        manufactory_required = (1 << 3),

        // Do not show this room in the construction menu for tutorial mode
        disabled_in_tutorials = (1 << 4),

        // Locked unless you complete an achievement
        locked_by_default = (1 << 5),

        // Do not render a roof tile above this room
        roof_hidden = (1 << 6),

        // Render a chimney above this room, or above the highest room sitting
        // atop of this one
        has_chimney = (1 << 7),

        // Do not render a chimney above this room in any circumstances
        disallow_chimney = (1 << 8),

        // Takes ion damage
        accepts_ion_damage = (1 << 9),

        // Fizzles ion damage
        cancels_ion_damage = (1 << 10),

        // The engine is allowed to render a flagpole on top of this room
        flag_mount = (1 << 11),

        // If hit by a weapon with damage that exceeds the room's health, the
        // colliding projectile will not be destroyed.
        fragile = (1 << 12),

        // Only available in adventure mode.
        adventure_mode_only = (1 << 13),

        // Only constructible in sandbox mode. Some of these blocks may appear
        // in advenutre mode, but not be constructible.
        only_constructible_in_sandbox = (1 << 14),

        // Behaves as a fluid (water, lava, etc.)
        fluid = (1 << 15),

        // No explosion or sound effects when the room health drops to zero.
        destroy_quietly = (1 << 16),

        // Cannot be salvaged.
        salvage_disallowed = (1 << 17),

        // Unsupported in multiplayer.
        multiplayer_unsupported = (1 << 18),

        // Unsupported in skyland forever.
        skyland_forever_unsupported = (1 << 19),

        // Cannot catch fire.
        fireproof = (1 << 20),

        // Fire will easily spread to this block.
        highly_flammable = (1 << 21),

        // Characters may move into this room. You may still need to override
        // plot_walkable_zones, in some cases.
        habitable = (1 << 22),

        // Melts nearby ice blocks.
        generates_heat = (1 << 23),

        oversize_explosion = (1 << 24),

        // Available in multiboot games. Unused...
        // I had this idea to support single cartridge
        // multiplayer. But... although it'd be possible to build, not worth the
        // trouble IMO.
        multiboot_compatible = (1 << 25),

        singleton = (1 << 26),

        human_only = (1 << 27),
        sylph_only = (1 << 28),
        goblin_only = (1 << 29),
    };
};



using AttackTargetPriority = Fixnum;
using ATP = AttackTargetPriority;



constexpr ATP operator"" _atp(long double value)
{
    return Fixnum((static_cast<float>(value)));
}



class Room : public Block
{
public:
    enum class Category {
        wall,
        weapon,
        factory,
        power,
        misc,
        decoration,
        count
    };


    static int default_health()
    {
        return 10;
    }


    static int default_cost()
    {
        return 10;
    }


    static int default_power()
    {
        return 10;
    }


    virtual ~Room();



    using Icon = u16;



    Room(Island* parent, const char* name, const RoomCoord& position);


    Room(const Room&) = delete;


    virtual bool add_occupant(EntityRef<Character> entity)
    {
        ready();
        characters_.push(std::move(entity));
        return true;
    }


    virtual void render_interior(App* app, TileId buffer[16][16]) = 0;
    virtual void render_exterior(App* app, TileId buffer[16][16]) = 0;

    virtual void render_scaffolding(TileId buffer[16][16]);

    virtual void render_cloak(TileId buffer[16][16])
    {
    }


    virtual bool cloaks_coordinate(const RoomCoord& c)
    {
        return false;
    }


    void init_ai_awareness();


    template <typename T> T* cast()
    {
        // Basically, there are some obscure cases where I need to downcast
        // stuff, and RTTI, in its current implementation, was causing issues on
        // GBA.
        if (str_eq(T::name(), name())) {
            return reinterpret_cast<T*>(this);
        }
        return nullptr;
    }


    virtual Weapon* cast_weapon();


    virtual void update(Time delta);
    virtual void rewind(Time delta);

    virtual void display(Platform::Screen& screen);


    virtual bool opponent_display_on_hover() const;


    virtual void display_on_hover(Platform::Screen& screen,
                                  const RoomCoord& cursor);


    Island* other_island() const;


    RoomCoord position() const
    {
        return {x_position_, y_position_};
    }


    Vec2<u8> size() const
    {
        return {size_x_, size_y_};
    }


    // A special method intended mainly for the rewind implementation. Invoked
    // when the rewind logic encounters an event indicating that a room finished
    // reloading.
    virtual void ___rewind___finished_reload()
    {
    }


    // A special method intended mainly for the rewind implementation. Invoked
    // when the rewind logic encounters an event indicating that a room used
    // its special ability. Required to correctly set reload timers.
    virtual void ___rewind___ability_used()
    {
    }


    virtual ScenePtr setup();


    // Block may be selected and activated even by the other player.
    virtual bool non_owner_selectable() const;


    ScenePtr select(const RoomCoord& cursor);


    virtual Optional<RoomCoord> get_target() const
    {
        return {};
    }


    virtual bool target_pinned() const;


    using TargetCount = u8;
    virtual TargetCount target_count() const;


    virtual void set_target(const TargetQueue& q, bool pinned);


    virtual void set_target(const RoomCoord& target, bool pinned);


    virtual void unset_target()
    {
    }


    virtual void on_level_start();


    virtual void project_deflector_shield();


    virtual Time reload_time_remaining() const
    {
        return 0;
    }


    virtual Time reload_interval() const
    {
        return 1;
    }


    virtual void override_reload_timer(Time new_time)
    {
        // ...
    }


    static Category category()
    {
        return Category::misc;
    }


    bool is_decoration() const;


    virtual void finalize();


    void __unsafe__ignore_finalizer()
    {
        finalized_ = true;
    }


    virtual void on_salvage();


    // NOTE: The first three elements of the result list must be room name
    // symbol, x, y. Subsequent entries in the list can contain whatever you
    // want.
    virtual lisp::Value* serialize();
    virtual void deserialize(lisp::Value*);


    virtual void append_name_suffix(StringBuffer<32>& result)
    {
        return;
    }


    Health health() const
    {
        return health_;
    }


    Health max_health() const;


    static Health health_upper_limit()
    {
        return std::numeric_limits<Health>::max();
    }


    void heal(Health amount);


    virtual void parent_layout_changed(RoomCoord moved_from, RoomCoord to);


    // Careful! Converts a room to an entirely new type, by changing the parent
    // class. If called from within a member function of a Room, the code would
    // need to return immediately after. Seriously, this function needs to be
    // treated with the utmost care if invoked from within a method of a derived
    // room, as this code invokes the existing room's destructor and constructs
    // a room of a new type in its place.
    //
    // NOTE: We allocate all rooms from the same memory pool, and we have
    // assertions elsewhere to check for alignment and stuff, so this code is
    // safe as long as you're careful when transmuting rooms from within derived
    // room code.
    void __unsafe__transmute(MetaclassIndex m);


    // DO NOT CALL __set_health()! Intended for rewind, multiplayer, and very
    // niche purposes.
    void __set_health(Health amount)
    {
        health_ = amount;
        update_description();
    }


    struct DamageConfiguration
    {
        bool ignore_deflector_shield_ = false;

        static DamageConfiguration create()
        {
            return {};
        }
    };


    virtual void apply_damage(
        Health damage,
        const DamageConfiguration& conf = DamageConfiguration::create());


    virtual void burn_damage(Health damage);


    void plunder(Health damage);
    void convert_to_plundered();


    void reset_injured_timer(Time value);


    virtual void plot_walkable_zones(bool matrix[16][16],
                                     Character* for_character);


    virtual void on_lightning()
    {
    }


    virtual void on_lightning_rewind()
    {
    }


    virtual Island* owner() const
    {
        return parent();
    }


    RoomMeta* metaclass() const;


    MetaclassIndex metaclass_index() const;


    const char* name() const;


    WorldCoord origin() const;


    WorldCoord center() const;


    WorldCoord visual_center() const;


    Island* parent() const
    {
        return parent_;
    }


    const EntityList<Character>& characters() const
    {
        return characters_;
    }


    EntityList<Character>& edit_characters()
    {
        return characters_;
    }


    enum class WeaponOrientation { none, horizontal, vertical };


    static WeaponOrientation weapon_orientation()
    {
        return WeaponOrientation::none;
    }


    virtual Power power_usage() const;


    static Icon icon()
    {
        return 3208;
    }


    static Icon unsel_icon()
    {
        return 2504;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        // TODO...
    }


    virtual bool description_visible();


    void dispatch_update(Room* next)
    {
        dispatch_list_ = next;
        dispatch_queued_ = true;
    }


    Room* dispatch_next() const
    {
        return dispatch_list_;
    }


    // IMPORTANT: only rooms that call ready() during their prior update call
    // will have their update() and display() member function invoked.
    void ready();


    virtual bool attach_drone(SharedEntityRef<Drone>);
    virtual void detach_drone(bool quiet);
    virtual Optional<SharedEntityRef<Drone>> drone() const;


    virtual bool create_replicant()
    {
        return false;
    }


    u8 default_palette();


    enum class Group {
        none,
        one,
        two,
        three,
        count,
    };


    void set_group(Group group);


    Group group() const;


    virtual void reset_state()
    {
    }


    virtual int debris_tile()
    {
        return 0;
    }


    virtual int debris_count()
    {
        return 3;
    }


    // Do not call directly! Use Island::move_room() instead.
    void __set_position(const RoomCoord& pos)
    {
        x_position_ = pos.x;
        y_position_ = pos.y;
        update_description();
    }

    ////////////////////////////////////////////////////////////////////////////
    //
    // Co-op synchronization:
    //
    // For co-op mode, to prevent two consoles from editing the same block at
    // once. Acquiring a lock broadcasts an acquire lock message, and the sender
    // should wait for a response from the other console before continuing. When
    // granting the lock to another connected game, the receiver should call
    // co_op_peer_acquire_lock(), which sets the lock bit to true, indicating
    // that another game acquired the lock. co_op_release_lock unsets the lock
    // bit and broadcasts a message to the other console, which should call
    // co_op_peer_release_lock(). While the semantics are admittedly a bit
    // complex, the alternative would be to allow both co-op players to set the
    // target of a weapon at the same time, which results in tons of bugs.
    //

    // When acquiring a lock, pass a lazy reference to a scene object, which the
    // game will enter after successfully acquiring the lock.
    //
    // Returns an intermediary scene which safely waits on the lock, if the lock
    // is available. If the lock is already locked, returns null_scene().
    //
    // Generally, co_op_acquire lock does almost everything for you, you just
    // need to make sure that the exit method of the deferred scene passed into
    // the function correctly releases the lock with co_op_release_lock()!

    ScenePtr co_op_acquire_lock(DeferredScene next);

    void co_op_release_lock();

    bool co_op_peer_acquire_lock();
    void co_op_peer_release_lock();


    bool co_op_locked() const;

    //
    ////////////////////////////////////////////////////////////////////////////


    void mark()
    {
        mark_bit_ = 1;
    }


    bool marked() const
    {
        return mark_bit_;
    }


    void unmark()
    {
        mark_bit_ = 0;
    }


    // The island class has a function, schedule_repaint(), allowing rooms to
    // request a full redraw when their tile graphics change. But a full repaint
    // isn't always necessary. Some tiles are joined with other tiles by the
    // island tile renderer, while some tiles are fully isolated and not joined
    // with any other tiles, and in those cases, there's no reason to redraw the
    // entire island's tile layer.
    void schedule_repaint();


    bool poll_repaint()
    {
        bool ret = partial_repaint_flag_;
        partial_repaint_flag_ = false;
        return ret;
    }


    // Whether an AI can see this room. Relevant for rooms covered by cloaking
    // fields. For a cloaked room, an AI will become aware of a room's identity
    // under certain conditions.
    bool ai_aware() const
    {
        return ai_aware_;
    }


    void set_ai_aware(bool ai_aware);


    // Players don't expect the AI algo to be running while the game is paused,
    // but it does. Re-init ai awareness of cloaked structures created during
    // pause upon unpause.
    void init_ai_awareness_upon_unpause()
    {
        init_awareness_upon_unpause_ = true;
    }


    bool should_init_ai_awareness_upon_unpause() const
    {
        return init_awareness_upon_unpause_;
    }


    bool visually_cloaked() const
    {
        return cloaked_;
    }


    void set_visually_cloaked(bool cloaked)
    {
        cloaked_ = cloaked;
        update_description();
    }


    ATP get_atp() const;


    void set_hidden(bool h)
    {
        hidden_ = h;
    }


    // Required in special cases for re-arranging blocks.
    bool hidden() const
    {
        return hidden_;
    }


    bool description_changed() const
    {
        return description_changed_;
    }


    void reset_description_changed()
    {
        description_changed_ = false;
    }


    void update_description()
    {
        description_changed_ = true;
    }


    virtual const char* upgrade_mt_name() const;


    bool is_powered_down() const
    {
        return powerdown_;
    }


    void set_powerdown(bool powerdown);


    virtual bool allows_powerdown();


    void set_shielded(bool shielded);


    virtual void amplify(bool enabled);


protected:
    void set_injured();


    ScenePtr do_select();

    virtual ScenePtr select_impl(const RoomCoord& cursor);


    virtual void on_powerchange()
    {
    }


private:
    // NOTE: All of these parameters are arranged carefully to use minimal
    // space. Do not go re-arranging things unless you understand how struct
    // packing works. Generally, packing only matters for 32 bit systems. For 64
    // bit build targets, ignore the comments below about field sizes, as we
    // don't need to worry about memory usage on systems with 64 bit CPUs.


    ////////////////////////////////////////////////////////////////////////////
    //
    // Four byte fields (assuming 32-bit):
    //
    ////////////////////////////////////////////////////////////////////////////

    Island* parent_;
    EntityList<Character> characters_;
    Time injured_timer_ = 0;

    // Many rooms sit around doing nothing most of the time. Each island
    // maintains a dispatch list of rooms, for which it'll run update code.
    Room* dispatch_list_;


    ////////////////////////////////////////////////////////////////////////////
    //
    // Two byte fields:
    //
    ////////////////////////////////////////////////////////////////////////////

    Health health_;

    ////////////////////////////////////////////////////////////////////////////
    //
    // Bitfields and single bytes:
    //
    ////////////////////////////////////////////////////////////////////////////

    // I originally used a u16 throughout the engine for representing metaclass
    // index. But, in practice, we don't have nearly 255 different blocks in the
    // game. While I still want to use a u16 in the main engine code, just in
    // case, I'm using a u8 in the room object metadata, and will move to a
    // larger datatype if I ever have more than 255 unique rooms for some
    // reason. I have assertions in the code to check overflow.
    using SmallMetaclassIndex = u8;

    SmallMetaclassIndex metaclass_index_;

    // Room's position. We only support coordinates 0-16, as even castles 13
    // tiles wide get to be too large for the game to be fun.
    u8 x_position_ : 4;
    u8 y_position_ : 4;

    u8 size_x_ : 4;
    u8 size_y_ : 4;

    [[maybe_unused]] u8 unused___ : 3;

    u8 shielded_ : 1;

    u8 finalized_ : 1;
    u8 dispatch_queued_ : 1;

    u8 group_ : 2;

    u8 co_op_locked_ : 1;
    u8 mark_bit_ : 1;

    u8 partial_repaint_flag_ : 1;

    u8 ai_aware_ : 1;
    u8 cloaked_ : 1;

    u8 init_awareness_upon_unpause_ : 1;

    u8 hidden_ : 1;
    u8 description_changed_ : 1;

    u8 accumulated_damage_ = 0;
    u8 show_damage_delay_frames_ : 6;

    u8 powerdown_ : 1;
    [[maybe_unused]] u8 unused_ : 1;
};



template <typename T> using RoomPtr = UniquePtr<T, void (*)(Room*)>;



} // namespace skyland
