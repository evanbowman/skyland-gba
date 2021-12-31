#pragma once



#include "bulkAllocator.hpp"
#include "graphics/sprite.hpp"
#include "health.hpp"
#include "hitbox.hpp"
#include "list.hpp"
#include "memory/buffer.hpp"
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
        sprite_.set_priority(1);
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


    virtual void on_collision(Platform& pfrm, App& app, Entity& other)
    {
    }


    void kill()
    {
        health_ = 0;
    }


    void apply_damage(Health amount)
    {
        health_ = std::max(0, health_ - amount);
    }


    Health health() const
    {
        return health_;
    }


protected:
    Sprite sprite_;
    HitBox hitbox_;
    Health health_;
};



static constexpr const int entity_pool_size = 64;
static constexpr const int entity_pool_align = 8;
static constexpr const int max_entity_size = 100;


struct EntityPools {
public:
    static const auto entities_per_pool = 19;
    static const auto pool_count = entity_pool_size / entities_per_pool;


    using EntityPool =
        Pool<max_entity_size, entities_per_pool, entity_pool_align>;


    void init(Platform& pfrm)
    {
        for (u32 i = 0; i < pools_.capacity(); ++i) {
            pools_.push_back(allocate_dynamic<EntityPool>(pfrm));
        }
    }


    void* get()
    {
        for (auto& pl : pools_) {
            if (not pl->empty()) {
                return pl->get();
            }
        }
        return nullptr;
    }


    void post(void* e)
    {
        for (auto& pl : pools_) {
            if (e >= pl->cells().begin() and e < pl->cells().end()) {
                pl->post((byte*)e);
                return;
            }
        }
        while (true) {
            // hmm... someone did something very bad. This entity wasn't
            // allocated from a pool in the first place...
        }
    }


private:
    Buffer<DynamicMemory<EntityPool>, pool_count> pools_;
};


template <typename T> using EntityRef = std::unique_ptr<T, void (*)(Entity*)>;



template <typename T> using SharedEntityRef = Rc<T, IntrusiveRcControlBlock<T>>;

template <typename T> using WeakEntityRef = Weak<T, IntrusiveRcControlBlock<T>>;



using EntityNode = BiNode<EntityRef<Entity>>;



template <u32 Capacity>
using EntityNodePool = Pool<sizeof(EntityNode), Capacity, alignof(Entity)>;



template <typename T>
using EntityList = List<EntityRef<T>, EntityNodePool<entity_pool_size>>;



template <typename T>
using SharedEntityList =
    List<SharedEntityRef<T>, EntityNodePool<entity_pool_size>>;


template <typename T>
using WeakEntityList = List<WeakEntityRef<T>, EntityNodePool<entity_pool_size>>;



template <typename T>
void update_entities(Platform& pfrm,
                     App& app,
                     Microseconds dt,
                     EntityList<T>& lat)
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
