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

#define COLD [[gnu::cold]]
#define HOT [[gnu::hot]]

#ifdef __GNUC__
#define UNLIKELY(COND) __builtin_expect((COND), false)
#else
#define UNLIKELY(COND) (COND)
#endif

#include <iterator>


#if defined(__GBA__) or defined(__NDS__)
#define READ_ONLY_DATA __attribute__((section(".rodata")))
#else
#define READ_ONLY_DATA
#endif


namespace _detail
{

template <typename T> struct reversion_wrapper
{
    T& iterable;
};


template <typename T> auto begin(reversion_wrapper<T> w)
{
    return std::rbegin(w.iterable);
}


template <typename T> auto end(reversion_wrapper<T> w)
{
    return std::rend(w.iterable);
}


} // namespace _detail


template <typename T> _detail::reversion_wrapper<T> reversed(T&& iterable)
{
    return {iterable};
}
