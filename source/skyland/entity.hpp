////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    virtual void update(Platform&, App&, Microseconds delta) = 0;


    virtual void rewind(Platform& pfrm, App& app, Microseconds delta)
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


    virtual void apply_damage(Platform& pfrm, App& app, Health amount)
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



template <typename T>
void rewind_entities(Platform& pfrm,
                     App& app,
                     Microseconds dt,
                     EntityList<T>& lat)
{
    for (auto it = lat.begin(); it not_eq lat.end();) {
        if (not(*it)->alive()) {
            it = lat.erase(it);
        } else {
            (*it)->rewind(pfrm, app, dt);
            ++it;
        }
    }
}



} // namespace skyland
