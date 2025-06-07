////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "memory/pool.hpp"
#include "scene.hpp"



namespace skyland
{



namespace scene_pool
{


#if defined(__GBA__) or defined(__NDS__)
static constexpr const int max_scene_size = 350;
#else
static constexpr const int max_scene_size = 700;
#endif
static constexpr const int pool_capacity = 3;



using _Pool = Pool<max_scene_size, pool_capacity, 8>;

extern _Pool* pool_;



inline void deleter(Scene* scene)
{
    if (scene) {
        scene->~Scene();
        pool_->free(reinterpret_cast<u8*>(scene));
    }
}



} // namespace scene_pool



template <typename T, typename... Args>
UniqueScenePtr<T> make_scene(Args&&... args)
{
    static_assert(sizeof(T) <= scene_pool::max_scene_size);
    static_assert(alignof(T) <= scene_pool::_Pool::alignment());

    if (scene_pool::pool_ == nullptr) {
        return {nullptr, scene_pool::deleter};
    }

    if (auto mem = scene_pool::pool_->alloc()) {
        new (mem) T(std::forward<Args>(args)...);

        return {reinterpret_cast<T*>(mem), scene_pool::deleter};
    }

    return {nullptr, scene_pool::deleter};
}



template <typename S, typename... Args>
DeferredScene make_deferred_scene(Args&&... args)
{
    return [args = std::make_tuple(std::forward<Args>(args)...)] {
        return std::apply([](auto&&... args) { return make_scene<S>(args...); },
                          std::move(args));
    };
}



} // namespace skyland
