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

#include "number/numeric.hpp"
#include <array>
#include <new>



class Platform;



class GenericPool
{
public:
    GenericPool(const char* name) : name_(name)
    {
        next_ = instances_;
        instances_ = this;
    }


    virtual ~GenericPool()
    {
        GenericPool* instance_list = instances_;
        GenericPool* prev = nullptr;

        while (instance_list) {
            if (instance_list == this) {
                if (prev) {
                    prev->next_ = next_;
                } else {
                    instances_ = next_;
                }
                return;
            }
            instance_list = instance_list->next_;
        }
    }


    virtual u32 pooled_element_size() const = 0;
    virtual u32 pooled_element_align() const = 0;
    virtual u32 pooled_element_count() const = 0;
    virtual u32 pooled_element_remaining() const = 0;


    const char* name() const
    {
        return name_;
    }


    static GenericPool* instances()
    {
        return instances_;
    }

    GenericPool* next() const
    {
        return next_;
    }

    static void print_diagnostics();

private:
    const char* name_;
    GenericPool* next_;

    static GenericPool* instances_;
};



template <u32 size, u32 count, u32 align = size> class Pool : public GenericPool
{
public:
    struct Cell
    {
        alignas(align) std::array<u8, size> mem_;
        Cell* next_;
    };

    using Cells = std::array<Cell, count>;


    u32 pooled_element_size() const override
    {
        return size;
    }


    u32 pooled_element_align() const override
    {
        return align;
    }


    u32 pooled_element_count() const override
    {
        return count;
    }


    u32 pooled_element_remaining() const override
    {
        return remaining();
    }


    Pool(const char* name) : GenericPool(name), freelist_(nullptr)
    {

        for (decltype(count) i = 0; i < count; ++i) {
            Cell* next = &cells_[i];
            next->next_ = freelist_;
            freelist_ = next;
        }
    }

    Pool(const Pool&) = delete;

    u8* alloc()
    {
        if (freelist_) {
            const auto ret = freelist_;
            freelist_ = freelist_->next_;
            return (u8*)ret;
        } else {
            return nullptr;
        }
    }

    u8* alloc_init(u8 fill_byte)
    {
        if (auto mem = alloc()) {
            memset(mem, fill_byte, size);
            return mem;
        }
        return nullptr;
    }

    void free(u8* mem)
    {
        auto cell = (Cell*)mem;
        cell->next_ = freelist_;
        freelist_ = cell;
    }

    static constexpr u32 element_size()
    {
        return size;
    }

    static constexpr u32 alignment()
    {
        return align;
    }

    Cells& cells()
    {
        return cells_;
    }

    bool is_freed(const Cell* cell) const
    {
        auto list = freelist_;
        while (list) {
            if (list == cell) {
                return true;
            }
            list = list->next_;
        }
        return false;
    }

    u32 remaining() const
    {
        const Cell* current = freelist_;
        int n = 0;
        while (current) {
            current = current->next_;
            ++n;
        }
        return n;
    }

    bool empty() const
    {
        return freelist_ == nullptr;
    }

private:
    Cells cells_;
    Cell* freelist_;
};


template <typename T, u32 count> class ObjectPool
{
public:
    template <typename... Args> T* alloc(Args&&... args)
    {
        auto mem = pool_.alloc();
        if (mem) {
            new (mem) T(std::forward<Args>(args)...);
            return reinterpret_cast<T*>(mem);
        } else {
            return nullptr;
        }
    }

    ObjectPool(const char* name) : pool_(name)
    {
    }

    void free(T* obj)
    {
        obj->~T();
        pool_.free((u8*)obj);
    }

    u32 remaining() const
    {
        return pool_.remaining();
    }

    bool empty() const
    {
        return pool_.empty();
    }

    using _Pool = Pool<sizeof(T), count, alignof(T)>;
    using Cells = typename _Pool::Cells;

    Cells& cells()
    {
        return pool_.cells();
    }

    bool is_freed(const typename _Pool::Cell* cell) const
    {
        return pool_.is_freed(cell);
    }

    template <typename F> void scan_cells(F&& callback)
    {
        auto& mem = pool_.cells();
        for (auto& cell : mem) {
            T* obj = reinterpret_cast<T*>(cell.mem_.data());
            callback(obj);
        }
    }

private:
    _Pool pool_;
};
