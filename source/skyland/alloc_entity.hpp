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

    void update(Time delta) override
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
Optional<SharedEntityRef<Base>> alloc_shared_entity(Args&&... args)
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
