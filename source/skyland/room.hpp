#pragma once

#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "health.hpp"
#include "metaclassIndex.hpp"
#include "number/numeric.hpp"
#include "platform/layer.hpp"
#include "power.hpp"
#include "scene.hpp"
#include <memory>



class Platform;



namespace skyland {



class App;
class Entity;
class Island;
class RoomMeta;
class BasicCharacter;
class Drone;



struct RoomProperties {
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
    };
};



class Room {
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


    void set_injured(Platform& pfrm);


    virtual void update(Platform& pfrm, App&, Microseconds delta);
    virtual void rewind(Platform& pfrm, App&, Microseconds delta);

    virtual void display(Platform::Screen& screen);


    Island* other_island(App&);


    const Vec2<u8>& position() const
    {
        return position_;
    }


    Vec2<u8>& __position()
    {
        return position_;
    }


    Vec2<u8> size() const
    {
        return {size_x_, size_y_};
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


    virtual ScenePtr<Scene> select(Platform& pfrm, App&);


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


    Health health() const
    {
        return health_;
    }


    Health max_health() const;


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


    virtual bool attach_drone(Platform&, App&, SharedEntityRef<Drone>);
    virtual void detach_drone(Platform&, App&, bool quiet);
    virtual std::optional<SharedEntityRef<Drone>> drone() const;


    virtual bool create_replicant(Platform& pfrm, App& app)
    {
        return false;
    }


    u8 default_palette();


protected:
    ScenePtr<Scene> do_select(Platform& pfrm, App& app);


private:
    Island* parent_;
    EntityList<BasicCharacter> characters_;

    // NOTE: we're a little tight on memory on some consoles, especially when
    // players create a lot of rooms. Given the game's mechanics, there's
    // practically no reason to support rooms larger than 7x7. No room structure
    // in the game even comes close to this size.
    u8 size_x_ : 3;
    u8 size_y_ : 3;

    u8 finalized_ : 1;
    u8 dispatch_queued_ : 1;

    // NOTE: These flags are reserved for stuff unique to a specific instance of
    // a room. If you want to set properties for an entire class of rooms, use
    // RoomProperties, and retrieve the field from the metaclass.
    u8 reserved_flags0_ : 8;

    Vec2<u8> position_;
    Health health_;
    MetaclassIndex metaclass_index_;

    Microseconds injured_timer_;

    // Many rooms sit around doing nothing most of the time. Each island
    // maintains a dispatch list of rooms, for which it'll run update code.
    Room* dispatch_list_;
};



template <typename T> using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



} // namespace skyland
