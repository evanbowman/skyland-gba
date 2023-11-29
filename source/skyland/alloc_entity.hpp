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



#include "globals.hpp"
#include "memory/rc.hpp"



namespace skyland
{



inline void entity_deleter(Entity* entity)
{
    if (entity) {
        entity->~Entity();
        auto& pools = globals().entity_pools_;
        pools.free(reinterpret_cast<u8*>(entity));
    }
}



inline EntityRef<Entity> null_entity()
{
    return {nullptr, entity_deleter};
}


class EntityOOM : public Entity
{
public:
    EntityOOM() : Entity({})
    {
        kill();
    }

    void update(Microseconds delta) override
    {
    }
};


template <typename T, typename... Args>
EntityRef<T> alloc_entity(Args&&... args)
{
    auto& pool = globals().entity_pools_;

    static_assert(sizeof(T) <= max_entity_size);
    static_assert(alignof(T) <= entity_pool_align);

    if (auto mem = pool.alloc()) {
        new (mem) T(std::forward<Args>(args)...);

        return EntityRef<T>(reinterpret_cast<T*>(mem), entity_deleter);
    }

    return {nullptr, entity_deleter};
}



// NOTE: Our Rc<T> class technically supports upcasting, but does not support
// conversions between Rc<T> of different type. So you can allocate an Rc<Base>
// containing a Derived class, but you cannot create an Rc<Derived> that's
// convertible to an Rc<Base> at the moment. But in our use case, we don't need
// access to an Rc<Derived>; doing everything through the base class pointer is
// sufficent for our purposes.
template <typename T, typename Base, typename... Args>
std::optional<SharedEntityRef<Base>> alloc_shared_entity(Args&&... args)
{
    auto& pool = globals().entity_pools_;

    static_assert(sizeof(T) <= max_entity_size);
    static_assert(alignof(T) <= entity_pool_align);

    if (auto mem = reinterpret_cast<T*>(pool.alloc())) {
        new (mem) T(std::forward<Args>(args)...);

        mem->finalizer_hook_ = [](IntrusiveRcControlBlock<Base>* cb) {
            cb->data_->~Base();
            auto& pools = globals().entity_pools_;
            pools.free(reinterpret_cast<u8*>(cb->data_));
        };

        mem->data_ = mem;

        return SharedEntityRef<Base>(
            static_cast<IntrusiveRcControlBlock<Base>*>(mem));
    }

    return {};
}



} // namespace skyland
