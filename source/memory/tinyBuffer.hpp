////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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

#include "number/int.h"
#include <new>



template <typename T, u32 Capacity>
struct TinyBuffer
{
    alignas(T) std::array<u8, Capacity * sizeof(T)> mem_;
    u8 count_ = 0;


    TinyBuffer()
    {
    }


    TinyBuffer(const TinyBuffer& other) = delete;


    ~TinyBuffer()
    {
        clear();
    }


    bool full() const
    {
        return count_ == Capacity;
    }


    T& operator[](u32 index)
    {
        return ((T*)mem_.data())[index];
    }


    const T& operator[](u32 index) const
    {
        return ((T*)mem_.data())[index];
    }


    bool push_back(const T& elem)
    {
        if (full()) {
            return false;
        }
        new ((T*)mem_.data() + count_) T(elem);
        ++count_;
        return true;
    }


    T& back() const
    {
        return *((T*)mem_.data() + (count_ - 1));
    }


    void pop_back()
    {
        if (empty()) {
            return;
        }
        ((T*)mem_.data() + --count_)->~T();
    }


    u8 size() const
    {
        return count_;
    }


    bool empty() const
    {
        return size() == 0;
    }


    void clear()
    {
        while (count_) {
            pop_back();
        }
    }
};
