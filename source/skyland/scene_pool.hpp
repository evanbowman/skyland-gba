#pragma once

#include "memory/pool.hpp"
#include "scene.hpp"



namespace skyland {



namespace scene_pool {



static constexpr const int max_scene_size = 256;
static constexpr const int pool_capacity = 3;



using _Pool = Pool<max_scene_size, pool_capacity, 8>;

extern _Pool* pool_;



inline void deleter(Scene* scene)
{
    if (scene) {
        scene->~Scene();
        pool_->post(reinterpret_cast<byte*>(scene));
    }
}



template <typename T, typename... Args> ScenePtr<T> alloc(Args&&... args)
{
    static_assert(sizeof(T) <= max_scene_size);
    static_assert(alignof(T) <= pool_->alignment());

    if (pool_ == nullptr) {
        return {nullptr, deleter};
    }

    if (auto mem = pool_->get()) {
        new (mem) T(std::forward<Args>(args)...);

        return {reinterpret_cast<T*>(mem), deleter};
    }

    return {nullptr, deleter};
}



} // namespace scene_pool



} // namespace skyland
