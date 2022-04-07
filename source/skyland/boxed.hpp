////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
