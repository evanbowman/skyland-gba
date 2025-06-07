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


#include <type_traits>
#include <utility>


// Maybe you'll think this class is kind of dumb. But I wanted to be able to
// create duck-typed objects without using malloc. Boxed implements an in-place
// allocation, which may be swapped for an instance with a type derived from T.


namespace skyland
{


template <typename T, typename DefaultType, int mem = sizeof(T), int align = 8>
class Boxed
{
public:
    template <typename... Args> Boxed(Args&&... args)
    {
        static_assert(std::is_base_of<T, DefaultType>::value);
        static_assert(alignof(DefaultType) <= align);
        static_assert(sizeof(DefaultType) <= mem);
        new (mem_) DefaultType(std::forward<Args>(args)...);
    }


    template <typename U, typename... Args> void emplace(Args&&... args)
    {
        static_assert(std::is_base_of<T, U>::value);
        static_assert(alignof(U) <= align);
        static_assert(sizeof(U) <= mem);
        destroy();
        new (mem_) U(std::forward<Args>(args)...);
    }


    T& operator*() const
    {
        return *reinterpret_cast<const T*>(mem_);
    }


    T* operator->() const
    {
        return reinterpret_cast<const T*>(mem_);
    }


    T& operator*()
    {
        return *reinterpret_cast<T*>(mem_);
    }


    T* operator->()
    {
        return reinterpret_cast<T*>(mem_);
    }


    Boxed(const Boxed&) = delete;


    ~Boxed()
    {
        destroy();
    }

private:
    void destroy()
    {
        reinterpret_cast<T*>(mem_)->~T();
    }

    alignas(align) char mem_[mem];
};



} // namespace skyland
