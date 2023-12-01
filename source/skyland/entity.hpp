////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once



#include "allocator.hpp"
#include "containers/list.hpp"
#include "graphics/sprite.hpp"
#include "health.hpp"
#include "hitbox.hpp"
#include "memory/buffer.hpp"
#include "memory/segmentedPool.hpp"
#include "memory/uniquePtr.hpp"
#include "number/numeric.hpp"



class Platform;



namespace skyland
{



class App;
class Room;
class Drone;
class OffscreenWarning;



class Entity
{
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


    virtual void update(Microseconds delta) = 0;


    virtual void rewind(Microseconds delta)
    {
        Platform::fatal("rewind unimplemented for this entity!");
    }


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


    virtual void on_collision(Room& room, Vec2<u8> origin)
    {
    }


    virtual void on_collision(Entity& other)
    {
    }


    void kill()
    {
        health_ = 0;
    }


    virtual void apply_damage(Health amount)
    {
        health_ = std::max(0, health_ - amount);
    }


    Health health() const
    {
        return health_;
    }


    virtual Drone* cast_drone()
    {
        return nullptr;
    }


    virtual bool entity_oom_deletable() const
    {
        return true;
    }


    // B/C no RTTI
    virtual OffscreenWarning* cast_offscreen_warning()
    {
        return nullptr;
    }


protected:
    Sprite sprite_;
    HitBox hitbox_;
    Health health_;
};



static constexpr const int entity_pool_size = 84;
static constexpr const int entity_pool_align = 8;

#if defined(__GBA__) or defined(__NDS__)
static constexpr const int max_entity_size = 128;
#else
static constexpr const int max_entity_size = 200;
#endif



using EntityPools =
    SegmentedPool<max_entity_size, entity_pool_size, 14, entity_pool_align>;



template <typename T> using EntityRef = UniquePtr<T, void (*)(Entity*)>;



template <typename T> using SharedEntityRef = Rc<T, IntrusiveRcControlBlock<T>>;

template <typename T> using WeakEntityRef = Weak<T, IntrusiveRcControlBlock<T>>;



using EntityNode = BinaryNode<EntityRef<Entity>>;



template <u32 Capacity>
using EntityNodePool = Pool<sizeof(EntityNode), Capacity, alignof(Entity)>;



struct GlobalEntityListData
{
    using Pool = EntityNodePool<entity_pool_size>;

    Pool& pool() const;
};



template <typename T> struct GlobalEntityListDataImpl : GlobalEntityListData
{
    BinaryNode<T>* begin_;
};



template <typename T>
using EntityListData = ListData<T, EntityNodePool<entity_pool_size>>;


template <typename T>
using EntityList = List<EntityRef<T>, GlobalEntityListDataImpl<EntityRef<T>>>;



template <typename T>
using SharedEntityList =
    List<SharedEntityRef<T>, GlobalEntityListDataImpl<SharedEntityRef<T>>>;



template <typename T> void update_entities(Microseconds dt, EntityList<T>& lat)
{
    for (auto it = lat.begin(); it not_eq lat.end();) {
        if (not(*it)->alive()) {
            it = lat.erase(it);
        } else {
            (*it)->update(dt);
            ++it;
        }
    }
}



template <typename T> void rewind_entities(Microseconds dt, EntityList<T>& lat)
{
    for (auto it = lat.begin(); it not_eq lat.end();) {
        if (not(*it)->alive()) {
            it = lat.erase(it);
        } else {
            (*it)->rewind(dt);
            ++it;
        }
    }
}



} // namespace skyland
