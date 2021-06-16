#pragma once



#include "graphics/sprite.hpp"
#include "health.hpp"
#include "hitbox.hpp"
#include "list.hpp"
#include "number/numeric.hpp"
#include <memory>



class Platform;



namespace skyland {



class App;
class Room;



class Entity {
public:
    Entity(const HitBox::Dimension& dimension) : health_(1)
    {
        // By default, use sprite position as hitbox position. Derived entities
        // can assign it to something else if need be (if the spite position
        // and entity posiiton are not the same thing);
        hitbox_.position_ = &sprite_.position_;
        hitbox_.dimension_ = dimension;
    }


    Entity(const Entity&) = delete;


    virtual ~Entity()
    {
    }


    virtual void update(Platform&, App&, Microseconds delta) = 0;


    const Sprite& sprite() const
    {
        return sprite_;
    }


    const HitBox& hitbox()
    {
        return hitbox_;
    }


    bool alive() const
    {
        return health_ not_eq 0;
    }


    virtual void on_collision(Platform& pfrm, App& app, Room& room)
    {
    }


protected:
    void kill()
    {
        health_ = 0;
    }


    Sprite sprite_;
    HitBox hitbox_;
    Health health_;
};



static constexpr const int entity_pool_size = 64;
static constexpr const int entity_pool_align = 8;
static constexpr const int max_entity_size = 96;



template <typename T> using EntityRef = std::unique_ptr<T, void (*)(Entity*)>;



using EntityNode = BiNode<EntityRef<Entity>>;



template <u32 Capacity>
using EntityNodePool = Pool<sizeof(EntityNode), Capacity, alignof(Entity)>;



template <typename T>
using EntityList = List<EntityRef<T>, EntityNodePool<entity_pool_size>>;
using EntityPool = Pool<max_entity_size, entity_pool_size, 8>;




template <typename T>
void
update_entities(Platform& pfrm, App& app, Microseconds dt, EntityList<T>& lat)
{
    for (auto it = lat.begin(); it not_eq lat.end();) {
        if (not(*it)->alive()) {
            it = lat.erase(it);
        } else {
            (*it)->update(pfrm, app, dt);
            ++it;
        }
    }
}



} // namespace skyland
