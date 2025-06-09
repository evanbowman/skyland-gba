////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "platform/scratch_buffer.hpp"



namespace skyland
{



using ExtensionSlot = u16;



struct ExtensionRef
{
    u16 cell_;
};



void* extension_deref(ExtensionRef* ref);
bool extension_alloc(ExtensionRef* ref);
void extension_free(ExtensionRef* ref);



struct ExtensionStats
{
    int used;
};



ExtensionStats extension_stats();



template <typename T> struct ExtensionField
{
private:
    ExtensionRef ref_;

public:
    template <typename... Args> ExtensionField(Args&&... args)
    {
        if (not extension_alloc(&ref_)) {
            PLATFORM.fatal("failed to allocate extension!");
        }
        static_assert(sizeof(T) <= 32);
        static_assert(alignof(T) <= alignof(void*));
        static_assert(std::is_trivially_copyable_v<T>);
        new (extension_deref(&ref_)) T(std::forward<Args>(args)...);
    }


    ExtensionField(const ExtensionRef&) = delete;


    T* operator->()
    {
        return reinterpret_cast<T*>(extension_deref(&ref_));
    }


    T& operator*()
    {
        return *reinterpret_cast<T*>(extension_deref(&ref_));
    }


    ~ExtensionField()
    {
        (*this)->~T();
        extension_free(&ref_);
    }
};



} // namespace skyland
