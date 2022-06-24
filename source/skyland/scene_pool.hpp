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



template <typename T, typename... Args> ScenePtr<T> alloc(Args&&... args)
{
    static_assert(sizeof(T) <= max_scene_size);
    static_assert(alignof(T) <= pool_->alignment());

    if (pool_ == nullptr) {
        return {nullptr, deleter};
    }

    if (auto mem = pool_->alloc()) {
        new (mem) T(std::forward<Args>(args)...);

        return {reinterpret_cast<T*>(mem), deleter};
    }

    return {nullptr, deleter};
}



template <typename S, typename... Args>
DeferredScene make_deferred_scene(Args&&... args)
{
    return [args = std::make_tuple(std::forward<Args>(args)...)] {
        return std::apply([](auto&&... args) { return alloc<S>(args...); },
                          std::move(args));
    };
}



} // namespace scene_pool



} // namespace skyland
