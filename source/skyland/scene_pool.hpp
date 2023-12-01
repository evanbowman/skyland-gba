////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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
