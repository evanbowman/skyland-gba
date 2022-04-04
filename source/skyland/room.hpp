#pragma once

#include "block.hpp"
#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "health.hpp"
#include "metaclassIndex.hpp"
#include "number/numeric.hpp"
#include "platform/layer.hpp"
#include "power.hpp"
#include "scene.hpp"
#include "script/value.hpp"
#include <memory>



class Platform;



namespace skyland
{



class App;
class Entity;
class Island;
class RoomMeta;
class BasicCharacter;
class Drone;



struct RoomProperties
{
    enum Value : u32 {
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

        // Only available in sandbox mode.
        sandbox_mode_only = (1 << 14),

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
    };
};



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



    virtual ~Room();



    using Icon = u16;



    Room(Island* parent, const char* name, const Vec2<u8>& position);


    Room(const Room&) = delete;


    virtual bool add_occupant(EntityRef<BasicCharacter> entity)
    {
        ready();
        characters_.push(std::move(entity));
        return true;
    }


    virtual void render_interior(App& app, u8 buffer[16][16]) = 0;
    virtual void render_exterior(App& app, u8 buffer[16][16]) = 0;

    virtual void render_scaffolding(App& app, u8 buffer[16][16]);


    void set_injured(Platform& pfrm);


    virtual void update(Platform& pfrm, App&, Microseconds delta);
    virtual void rewind(Platform& pfrm, App&, Microseconds delta);

    virtual void display(Platform::Screen& screen);


    virtual void display_on_hover(Platform::Screen& screen,
                                  App& app,
                                  const Vec2<u8>& cursor);


    Island* other_island(App&);


    Vec2<u8> position() const
    {
        return {x_position_, y_position_};
    }


    Vec2<u8> size() const
    {
        return {u8(__packed_size_x_ + 1), u8(__packed_size_y_ + 1)};
    }


    // A special method intended mainly for the rewind implementation. Invoked
    // when the rewind logic encounters an event indicating that a room finished
    // reloading.
    virtual void ___rewind___finished_reload(Platform&, App&)
    {
    }


    // A special method intended mainly for the rewind implementation. Invoked
    // when the rewind logic encounters an event indicating that a room used
    // its special ability. Required to correctly set reload timers.
    virtual void ___rewind___ability_used(Platform&, App&)
    {
    }


    virtual ScenePtr<Scene> setup(Platform& pfrm, App&);


    // Block may be selected and activated even by the other player.
    virtual bool non_owner_selectable() const
    {
        return false;
    }


    virtual ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor);


    virtual void set_target(Platform& pfrm, App& app, const Vec2<u8>& target)
    {
    }


    virtual void unset_target(Platform& pfrm, App& app)
    {
    }


    virtual Microseconds reload_time_remaining() const
    {
        return 0;
    }


    static Category category()
    {
        return Category::misc;
    }


    virtual void finalize(Platform& pfrm, App& app);


    // NOTE: The first three elements of the result list must be room name
    // symbol, x, y. Subsequent entries in the list can contain whatever you
    // want.
    virtual lisp::Value* serialize();
    virtual void deserialize(lisp::Value*);


    Health health() const
    {
        return health_;
    }


    Health max_health() const;


    static Health health_upper_limit()
    {
        return std::numeric_limits<Health>::max();
    }


    void heal(Platform& pfrm, App& app, Health amount);


    // DO NOT CALL __set_health()! Intended for rewind, multiplayer, and very
    // niche purposes.
    void __set_health(Health amount)
    {
        health_ = amount;
    }


    virtual void apply_damage(Platform&, App&, Health damage);


    void plunder(Platform&, App&, Health damage);


    void reset_injured_timer(Microseconds value);


    virtual void plot_walkable_zones(App& app, bool matrix[16][16]);


    RoomMeta* metaclass() const;


    MetaclassIndex metaclass_index() const;


    const char* name() const;


    Vec2<Float> origin() const;


    Vec2<Float> center() const;


    Island* parent() const
    {
        return parent_;
    }


    EntityList<BasicCharacter>& characters()
    {
        return characters_;
    }


    virtual Power power_usage() const;


    static Power consumes_power()
    {
        return 10;
    }


    static RoomProperties::Value properties()
    {
        return RoomProperties::none;
    }


    static Icon icon()
    {
        return 450;
    }


    static Icon unsel_icon()
    {
        return 450;
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
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


    virtual bool attach_drone(Platform&, App&, SharedEntityRef<Drone>);
    virtual void detach_drone(Platform&, App&, bool quiet);
    virtual std::optional<SharedEntityRef<Drone>> drone() const;


    virtual bool create_replicant(Platform& pfrm, App& app)
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


    // Do not call directly! Use Island::move_room() instead.
    void __set_position(const Vec2<u8>& pos)
    {
        x_position_ = pos.x;
        y_position_ = pos.y;
    }


protected:
    ScenePtr<Scene> do_select(Platform& pfrm, App& app);


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
    EntityList<BasicCharacter> characters_;
    Microseconds injured_timer_ = 0;

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

    // NOTE: we're a little tight on memory on some consoles, especially when
    // players create a lot of rooms. Given the game's mechanics, there's
    // practically no reason to support rooms larger than 4x4. No room structure
    // in the game exceeds four tiles in either direction. NOTE: as zero-sized
    // rooms don't make sense, we interpret the size fields as off-by-one,
    // giving us a range of values from one to four.
    u8 __packed_size_x_ : 2;
    u8 __packed_size_y_ : 2;

    u8 finalized_ : 1;
    u8 dispatch_queued_ : 1;

    u8 group_ : 2;

    // Bytes freed up by various space optimization techniques.
    u8 reserved_[7];
};



template <typename T> using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



} // namespace skyland
