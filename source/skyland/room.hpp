#pragma once

#include "entity.hpp"
#include "health.hpp"
#include "number/numeric.hpp"
#include "platform/layer.hpp"
#include "scene.hpp"
#include <memory>



class Platform;



namespace skyland {



class App;
class Entity;
class Island;
class RoomMeta;
class Cannonball;



class Room {
public:
    virtual ~Room()
    {
    }


    Room(Island* parent,
         const char* name,
         const Vec2<u8>& size,
         const Vec2<u8>& position,
         Health health);


    virtual bool add_occupant(Entity* entity)
    {
        return false;
    }


    virtual void render_interior(Platform& pfrm, Layer layer) = 0;
    virtual void render_exterior(Platform& pfrm, Layer layer) = 0;


    void set_injured(Platform& pfrm);


    virtual void update(Platform& pfrm, App&, Microseconds delta);


    Island* other_island(App&);


    virtual bool has_roof()
    {
        return true;
    }


    virtual bool has_chimney()
    {
        return false;
    }


    const Vec2<u8>& position() const
    {
        return position_;
    }


    const Vec2<u8>& size() const
    {
        return size_;
    }


    virtual ScenePtr<Scene> select(Platform& pfrm);


    virtual void set_target(const Vec2<u8>& target)
    {
    }


    void on_collision(Platform& pfrm, App& app, Entity& entity);


    void apply_damage(Health damage)
    {
        if (damage > health_) {
            health_ = 0;
        } else {
            health_ -= damage;
        }
    }


    virtual void plot_walkable_zones(bool matrix[16][16]);


    RoomMeta* metaclass()
    {
        return metaclass_;
    }


    Vec2<Float> origin() const;


    Vec2<Float> center() const;


    Island* parent() const
    {
        return parent_;
    }


    EntityList& characters()
    {
        return characters_;
    }


    Health health() const
    {
        return health_;
    }


private:
    Island* parent_;
    RoomMeta* metaclass_;
    EntityList characters_;
    Vec2<u8> size_;
    Vec2<u8> position_;
    Health health_;
    Microseconds injured_timer_;
};



template <typename T> using RoomPtr = std::unique_ptr<T, void (*)(Room*)>;



} // namespace skyland
