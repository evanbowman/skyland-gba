#pragma once



#include "number/numeric.hpp"
#include "graphics/sprite.hpp"
#include "list.hpp"
#include <memory>
#include "hitbox.hpp"



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
    u16 health_;
};



static constexpr const int entity_pool_size = 64;
static constexpr const int entity_pool_align = 8;
static constexpr const int max_entity_size = 64;



template <typename T>
using EntityRef = std::unique_ptr<T, void (*)(Entity*)>;



using EntityNode = BiNode<EntityRef<Entity>>;



template <u32 Capacity>
using EntityNodePool = Pool<sizeof(EntityNode), Capacity, alignof(Entity)>;




using EntityList = List<EntityRef<Entity>, EntityNodePool<entity_pool_size>>;
using EntityPool = Pool<max_entity_size, entity_pool_size, 8>;



} // namespace skyland
