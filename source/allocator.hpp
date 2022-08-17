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

#include "platform/platform.hpp"
#include <memory>
#include <new>


// borrowed from standard library
inline void*
align(size_t __align, size_t __size, void*& __ptr, size_t& __space) noexcept
{
    const auto __intptr = reinterpret_cast<uintptr_t>(__ptr);
    const auto __aligned = (__intptr - 1u + __align) & -__align;
    const auto __diff = __aligned - __intptr;
    if ((__size + __diff) > __space)
        return nullptr;
    else {
        __space -= __diff;
        return __ptr = reinterpret_cast<void*>(__aligned);
    }
}



// An abstraction for a single large allocation. Must fit within 2K of
// memory. If your data is nowhere near 2k, and the data is long lived, might be
// better to use a bulk allocator, and share the underlying scratch buffer with
// other stuff.
template <typename T> struct DynamicMemory
{
    ScratchBufferPtr memory_;
    std::unique_ptr<T, void (*)(T*)> obj_;

    DynamicMemory() = default;
    DynamicMemory(ScratchBufferPtr mem, std::unique_ptr<T, void (*)(T*)> obj)
        : memory_(mem), obj_(std::move(obj))

    {
    }


    DynamicMemory(const DynamicMemory& other) = delete;


    template <typename U> DynamicMemory& operator=(DynamicMemory<U>&& rebind)
    {
        memory_ = rebind.memory_;
        obj_ = {rebind.obj_.release(), [](T* val) {
                    if (val) {
                        if constexpr (not std::is_trivial<T>()) {
                            val->~T();
                        }
                    }
                }};
        return *this;
    }


    template <typename U>
    DynamicMemory(DynamicMemory<U>&& rebind)
        : memory_(rebind.memory_),
          obj_(std::unique_ptr<T, void (*)(T*)>(
              rebind.obj_.release(),
              [](T* val) {
                  if (val) {
                      if constexpr (not std::is_trivial<T>()) {
                          val->~T();
                      }
                  }
              }))
    {
    }

    T& operator*() const
    {
        return *obj_.get();
    }

    T* operator->() const
    {
        return obj_.get();
    }

    operator bool() const
    {
        return obj_ not_eq nullptr;
    }
};


template <typename T, typename... Args>
DynamicMemory<T> allocate_dynamic(const ScratchBuffer::Tag& tag, Args&&... args)
{
    static_assert(sizeof(T) + alignof(T) <= sizeof ScratchBuffer::data_);

    auto sc_buf = make_scratch_buffer(tag);

    auto deleter = [](T* val) {
        if (val) {
            if constexpr (not std::is_trivial<T>()) {
                val->~T();
            }
            // No need to actually deallocate anything, we
            // just need to make sure that we're calling the
            // destructor.
        }
    };

    void* alloc_ptr = sc_buf->data_;
    std::size_t size = sizeof sc_buf->data_;

    if (align(alignof(T), sizeof(T), alloc_ptr, size)) {
        T* result = reinterpret_cast<T*>(alloc_ptr);
        new (result) T(std::forward<Args>(args)...);
        alloc_ptr = (char*)alloc_ptr + sizeof(T);
        size -= sizeof(T);

        return {sc_buf, {result, deleter}};
    }
    return {sc_buf, {nullptr, deleter}};
}



// A lightweight, copyable, reference-counted large allocation. Stored data must
// be trivially destructible.
template <typename T> class ScratchMemory
{
private:
    // ScratchMemory stores the aligned offset of the data in the scratch buffer
    // in a single byte value in the first byte of the buffer.
    struct DataHeader
    {
        u8 data_offset_;
    };

    // Layout:
    // u8 data_offset_;
    // u8 alignment_padding_[...];
    // u8 data_[sizeof(T)];

public:
    ScratchMemory() : handle_(make_scratch_buffer("scratch-memory"))
    {
        void* alloc_ptr = handle_->data_;
        std::size_t size = sizeof handle_->data_;

        auto header = (DataHeader*)alloc_ptr;

        alloc_ptr = (u8*)alloc_ptr + sizeof(DataHeader);
        size -= sizeof(DataHeader);

        if (align(alignof(T), sizeof(T), alloc_ptr, size)) {
            T* result = reinterpret_cast<T*>(alloc_ptr);
            new (result) T();
            alloc_ptr = (char*)alloc_ptr + sizeof(T);
            size -= sizeof(T);

            header->data_offset_ = (u8*)result - (u8*)header;

        } else {
            Platform::fatal("ScratchData does not fit in ScratchBuffer");
        }
    }


    // And here's the downside: addition operation required whenever accessing
    // ScratchMemory contents. Best to dereference once and store the resulting
    // T& on the stack.
    T& operator*() const
    {
        u8* data = (u8*)handle_->data_;
        auto header = (DataHeader*)data;
        data += header->data_offset_;
        return *reinterpret_cast<T*>(data);
    }


    // Intentionally unimplemented, see description alongside operator*.
    // T* operator->() const
    // {
    //     return **this;
    // }


    ScratchMemory(const ScratchMemory& other) : handle_(other.handle_)
    {
    }


    static_assert(std::is_trivially_destructible<T>());


private:
    ScratchBufferPtr handle_;
};



// Does not provide any mechanism for deallocation. Everything allocated from
// the memory region will be de-allocated at once when the allocator goes out of
// scope and lets go of its buffer.
struct ScratchBufferBulkAllocator
{

    ScratchBufferBulkAllocator()
        : buffer_(make_scratch_buffer("bulk-allocator")),
          alloc_ptr_(buffer_->data_), size_(sizeof buffer_->data_)
    {
    }

    ScratchBufferBulkAllocator(ScratchBufferBulkAllocator&&) = default;
    ScratchBufferBulkAllocator(const ScratchBufferBulkAllocator&) = delete;

    template <typename T> using Ptr = std::unique_ptr<T, void (*)(T*)>;

    template <typename T> static Ptr<T> null()
    {
        auto deleter = [](T* val) {
            if (val) {
                val->~T();
                // No need to actually deallocate anything, we
                // just ean to make sure that we're calling the
                // destructor.
            }
        };
        return {nullptr, deleter};
    }

    template <typename T, typename... Args> Ptr<T> alloc(Args&&... args)
    {
        auto deleter = [](T* val) {
            if (val) {
                val->~T();
                // No need to actually deallocate anything, we
                // just ean to make sure that we're calling the
                // destructor.
            }
        };

        if (align(alignof(T), sizeof(T), alloc_ptr_, size_)) {
            T* result = reinterpret_cast<T*>(alloc_ptr_);
            new (result) T(std::forward<Args>(args)...);
            alloc_ptr_ = (char*)alloc_ptr_ + sizeof(T);
            size_ -= sizeof(T);
            return {result, deleter};
        }

        return {nullptr, deleter};
    }

private:
    ScratchBufferPtr buffer_;
    void* alloc_ptr_;
    std::size_t size_;
};


template <u8 pages> struct BulkAllocator
{
    BulkAllocator(Platform& pfrm)
    {
        if (scratch_buffers_remaining() < pages) {
            warning(pfrm, "available scratch buffer count may be too low!");
        }
        buffers_.emplace_back();
    }

    template <typename T, typename... Args> auto alloc(Args&&... args)
    {
        auto try_alloc = [&] {
            return buffers_.back().template alloc<T>(
                std::forward<Args>(args)...);
        };

        if (auto mem = try_alloc()) {
            return mem;
        } else {
            if (buffers_.full()) {
                return ScratchBufferBulkAllocator::null<T>();
            } else {
                buffers_.emplace_back();
                if (auto mem = try_alloc()) {
                    return mem;
                } else {
                    return ScratchBufferBulkAllocator::null<T>();
                }
            }
        }
    }

private:
    Buffer<ScratchBufferBulkAllocator, pages> buffers_;
};
