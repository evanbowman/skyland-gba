#pragma once



#include "globals.hpp"



namespace skyland {



inline void entity_deleter(Entity* entity)
{
    if (entity) {
        entity->~Entity();
        auto& pool = std::get<SkylandGlobalData>(globals()).entity_pool_;
        pool.post(reinterpret_cast<byte*>(entity));
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
    auto& pool = std::get<SkylandGlobalData>(globals()).entity_pool_;

    static_assert(sizeof(T) <= max_entity_size);
    static_assert(alignof(T) <= pool.alignment());

    if (auto mem = pool.get()) {
        new (mem) T(std::forward<Args>(args)...);

        return EntityRef<T>(reinterpret_cast<T*>(mem), entity_deleter);
    }

    return {nullptr, entity_deleter};
}



} // namespace skyland
