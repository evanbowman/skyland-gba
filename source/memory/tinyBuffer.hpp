////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "number/int.h"
#include <array>
#include <new>



// Base class with common implementation
template <typename T, u32 Capacity, bool TriviallyDestructible>
struct TinyBufferImpl
{
    alignas(T) std::array<u8, Capacity * sizeof(T)> mem_;
    u8 count_ = 0;
    using ValueType = T;

    T* data()
    {
        return (T*)mem_.data();
    }

    const T* data() const
    {
        return (const T*)mem_.data();
    }

    TinyBufferImpl() = default;

    static constexpr u8 capacity()
    {
        return Capacity;
    }

    TinyBufferImpl(const TinyBufferImpl&) = default;
    TinyBufferImpl& operator=(const TinyBufferImpl&) = default;

    bool full() const
    {
        return count_ == Capacity;
    }

    T& operator[](u32 index)
    {
        return data()[index];
    }

    const T& operator[](u32 index) const
    {
        return data()[index];
    }

    bool push_back(const T& elem)
    {
        if (full()) {
            return false;
        }
        new (data() + count_) T(elem);
        ++count_;
        return true;
    }

    T& back()
    {
        return data()[count_ - 1];
    }

    const T& back() const
    {
        return data()[count_ - 1];
    }

    void pop_back()
    {
        if (empty()) {
            return;
        }
        if constexpr (!TriviallyDestructible) {
            data()[--count_].~T();
        } else {
            --count_;
        }
    }

    u8 size() const
    {
        return count_;
    }

    bool empty() const
    {
        return count_ == 0;
    }

    void clear()
    {
        if constexpr (!TriviallyDestructible) {
            while (count_) {
                pop_back();
            }
        } else {
            count_ = 0;
        }
    }
};



// Specialization for trivially destructible types - no destructor
template <typename T, u32 Capacity>
struct TinyBufferImpl<T, Capacity, true>
{
    alignas(T) std::array<u8, Capacity * sizeof(T)> mem_;
    u8 count_ = 0;
    using ValueType = T;

    T* data()
    {
        return (T*)mem_.data();
    }

    const T* data() const
    {
        return (const T*)mem_.data();
    }

    TinyBufferImpl() = default;

    static constexpr u8 capacity()
    {
        return Capacity;
    }

    TinyBufferImpl(const TinyBufferImpl&) = default;
    TinyBufferImpl& operator=(const TinyBufferImpl&) = default;
    // No destructor!

    bool full() const
    {
        return count_ == Capacity;
    }

    T& operator[](u32 index)
    {
        return data()[index];
    }

    const T& operator[](u32 index) const
    {
        return data()[index];
    }

    bool push_back(const T& elem)
    {
        if (full()) {
            return false;
        }
        new (data() + count_) T(elem);
        ++count_;
        return true;
    }

    T& back()
    {
        return data()[count_ - 1];
    }

    const T& back() const
    {
        return data()[count_ - 1];
    }

    void pop_back()
    {
        if (!empty()) {
            --count_;
        }
    }

    u8 size() const
    {
        return count_;
    }

    bool empty() const
    {
        return count_ == 0;
    }

    void clear()
    {
        count_ = 0;
    }
};



// Specialization for non-trivially destructible types - has destructor
template <typename T, u32 Capacity>
struct TinyBufferImpl<T, Capacity, false>
{
    alignas(T) std::array<u8, Capacity * sizeof(T)> mem_;
    u8 count_ = 0;
    using ValueType = T;

    T* data()
    {
        return (T*)mem_.data();
    }

    const T* data() const
    {
        return (const T*)mem_.data();
    }

    TinyBufferImpl() = default;

    static constexpr u8 capacity()
    {
        return Capacity;
    }

    TinyBufferImpl(const TinyBufferImpl&) = default;
    TinyBufferImpl& operator=(const TinyBufferImpl&) = default;

    ~TinyBufferImpl()
    {
        clear();
    }

    bool full() const
    {
        return count_ == Capacity;
    }

    T& operator[](u32 index)
    {
        return data()[index];
    }

    const T& operator[](u32 index) const
    {
        return data()[index];
    }

    bool push_back(const T& elem)
    {
        if (full()) {
            return false;
        }
        new (data() + count_) T(elem);
        ++count_;
        return true;
    }

    T& back()
    {
        return data()[count_ - 1];
    }

    const T& back() const
    {
        return data()[count_ - 1];
    }

    void pop_back()
    {
        if (empty()) {
            return;
        }
        data()[--count_].~T();
    }

    u8 size() const
    {
        return count_;
    }

    bool empty() const
    {
        return count_ == 0;
    }

    void clear()
    {
        while (count_) {
            pop_back();
        }
    }
};

// Public interface
template <typename T, u32 Capacity>
using TinyBuffer = TinyBufferImpl<T, Capacity, std::is_trivially_destructible_v<T>>;
