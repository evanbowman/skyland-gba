#pragma once



#include "globals.hpp"
#include "memory/rc.hpp"



namespace skyland {



inline void entity_deleter(Entity* entity)
{
    if (entity) {
        entity->~Entity();
        auto& pools = std::get<SkylandGlobalData>(globals()).entity_pools_;
        pools.post(reinterpret_cast<byte*>(entity));
    }
}



inline EntityRef<Entity> null_entity()
{
    return {nullptr, entity_deleter};
}


class EntityOOM : public Entity {
public:
    EntityOOM() : Entity({})
    {
        kill();
    }

    void update(Platform&, App&, Microseconds delta) override
    {
    }
};


template <typename T, typename... Args>
EntityRef<T> alloc_entity(Args&&... args)
{
    auto& pool = std::get<SkylandGlobalData>(globals()).entity_pools_;

    static_assert(sizeof(T) <= max_entity_size);
    static_assert(alignof(T) <= entity_pool_align);

    if (auto mem = pool.get()) {
        new (mem) T(std::forward<Args>(args)...);

        return EntityRef<T>(reinterpret_cast<T*>(mem), entity_deleter);
    }

    return {nullptr, entity_deleter};
}



template <typename T, typename... Args>
std::optional<SharedEntityRef<T>> alloc_shared_entity(Args&&... args)
{
    auto& pool = std::get<SkylandGlobalData>(globals()).entity_pools_;

    if (auto mem = reinterpret_cast<T*>(pool.get())) {
        new (mem) T(std::forward<Args>(args)...);

        mem->finalizer_hook_ = [](IntrusiveRcControlBlock<T>* cb) {
            auto& pools = std::get<SkylandGlobalData>(globals()).entity_pools_;
            pools.post(reinterpret_cast<byte*>(cb->data_));
        };

        mem->data_ = mem;

        return SharedEntityRef<T>(
            static_cast<IntrusiveRcControlBlock<T>*>(mem));
    }

    return {};
}



} // namespace skyland
