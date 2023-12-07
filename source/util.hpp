////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
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
