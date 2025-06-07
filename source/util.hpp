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


template <typename T, typename U> bool contains(const T& t, const U& u)
{
    for (auto& v : t) {
        if (v == u) {
            return true;
        }
    }
    return false;
}



void logic_error(const char* file, int line);
#define LOGIC_ERROR() logic_error(__FILE__, __LINE__)
