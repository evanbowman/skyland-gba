#pragma once

#include "entity.hpp"
#include "entity/character/basicCharacter.hpp"
#include "health.hpp"
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
class Cannonball;
class BasicCharacter;



struct Conditions {
    enum Value : u32 {
        none = 0,
        workshop_required = (1 << 0),
        not_constructible = (1 << 1),
        plugin = (1 << 2),
        foundry_required = (1 << 3),
        disabled_in_tutorials = (1 << 4),
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



    static bool unlocked_by_default()
    {
        return true;
    }



    virtual bool add_occupant(EntityRef<BasicCharacter> entity)
    {
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


    virtual bool has_roof()
    {
        return true;
    }


    virtual bool disallow_chimney()
    {
        return false;
    }


    virtual bool has_chimney()
    {
        return false;
    }


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


    void on_collision(Platform& pfrm, App& app, Entity& entity);


    void apply_damage(Platform&, App&, Health damage);


    void plunder(Platform&, App&, Health damage);


    virtual void plot_walkable_zones(App& app, bool matrix[16][16]);


    RoomMeta* metaclass()
    {
        return metaclass_;
    }


    void reset_injured_timer(Microseconds value);


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


    Health health() const
    {
        return health_;
    }


    static Power consumes_power()
    {
        return 10;
    }


    static Conditions::Value conditions()
    {
        return Conditions::none;
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


    Health max_health() const;


    void heal(Platform& pfrm, App& app, Health amount);


    void __set_health(Health amount)
    {
        health_ = amount;
    }


    virtual bool description_visible();


protected:
    ScenePtr<Scene> do_select(Platform& pfrm, App& app);


private:
    Island* parent_;
    RoomMeta* metaclass_;
    EntityList<BasicCharacter> characters_;

    // NOTE: we're a little tight on memory on some consoles, especially when
    // players create a lot of rooms. Given the game's mechanics, there's
    // practically no reason to support rooms larger than 7x7. No room structure
    // in the game even comes close to this size.
    u8 size_x_ : 3;
    u8 size_y_ : 3;

    u8 finalized_ : 1;
    u8 reserved_flags1_ : 1;
    u8 reserved_flags0_ : 8;

    Vec2<u8> position_;
    Health health_;

    Microseconds injured_timer_;
};



template <typename T> using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



} // namespace skyland
