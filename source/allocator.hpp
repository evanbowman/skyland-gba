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

#include "memory/uniquePtr.hpp"
#ifndef __TEST__
#include "platform/platform.hpp"
#endif
#include "platform/scratch_buffer.hpp"
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
    UniquePtr<T, void (*)(T*)> obj_;

    DynamicMemory() = default;
    DynamicMemory(ScratchBufferPtr mem, UniquePtr<T, void (*)(T*)> obj)
        : memory_(mem), obj_(std::move(obj))

    {
    }


    DynamicMemory(const DynamicMemory& other) = delete;


    template <typename U> DynamicMemory& operator=(DynamicMemory<U>&& rebind)
    {
        memory_ = rebind.memory_;
        // Preserve the original deleter instead of creating a new one
        obj_ = {static_cast<T*>(rebind.obj_.release()),
                reinterpret_cast<void (*)(T*)>(rebind.obj_.get_deleter())};
        return *this;
    }

    template <typename U>
    DynamicMemory(DynamicMemory<U>&& rebind)
        : memory_(rebind.memory_),
          obj_(static_cast<T*>(rebind.obj_.release()),
               reinterpret_cast<void (*)(T*)>(rebind.obj_.get_deleter()))
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
DynamicMemory<T> allocate_dynamic_fast(const ScratchBuffer::Tag& tag,
                                       Args&&... args)
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



template <typename T, typename... Args>
DynamicMemory<T> allocate_dynamic(const ScratchBuffer::Tag& tag, Args&&... args)
{
    static_assert(sizeof(T) + alignof(T) <= sizeof ScratchBuffer::data_);

    auto sc_buf = make_zeroed_sbr(tag, sizeof(T) + alignof(T));

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
    ScratchMemory()
        : handle_(make_zeroed_sbr("scratch-memory", sizeof(T) + alignof(T)))
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

    template <typename T> using Ptr = UniquePtr<T, void (*)(T*)>;

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



template <u8 pages, typename T> class BumpAllocator
{
public:
    BumpAllocator()
    {
        mem_.emplace_back(allocate_dynamic_fast<Block>(
            "depth-block", typename Block::SkipZeroFill{}));
        current_ = &*mem_.back();
    }

    static constexpr int block_capacity()
    {
        return (SCRATCH_BUFFER_SIZE / sizeof(T)) - 2;
    }

    template <typename... Args> T* alloc(Args&&... args)
    {
        constexpr int cap = block_capacity();
        if (cnt_ == cap) {
            if (mem_.full()) {
                Platform::fatal("Bump allocator oom!");
            }
            mem_.emplace_back(allocate_dynamic_fast<Block>(
                "depth-block", typename Block::SkipZeroFill{}));
            current_ = &*mem_.back();
            cnt_ = 0;
        }

        ++cnt_;
        current_->emplace_unsafe(std::forward<Args>(args)...);
        return &current_->back();
    }

private:
    using Block = Buffer<T, block_capacity()>;
    Buffer<DynamicMemory<Block>, pages> mem_;
    Block* current_;
    int cnt_ = 0;
};



template <u32 max_pages> struct GenericBumpAllocator
{
    Buffer<ScratchBufferPtr, max_pages> pages_;
    ScratchBufferPtr current_page_;
    size_t current_page_remaining_ = 0;


    GenericBumpAllocator() : current_page_(make_zeroed_sbr("xml"))
    {
        pages_.emplace_back(current_page_);
        current_page_remaining_ = SCRATCH_BUFFER_SIZE;
    }


    const char* strdup(const char* str)
    {
        const auto len = strlen(str);
        if (auto cpy = (char*)alloc(len + 1, 1)) {
            for (u32 i = 0; i < len; ++i) {
                cpy[i] = str[i];
            }
            cpy[len] = '\0';
            return cpy;
        }
        return nullptr;
    }


    template <typename T> T* alloc()
    {
        return reinterpret_cast<T*>(alloc(sizeof(T), alignof(T)));
    }


    template <typename T> T* alloc_array(u32 count)
    {
        return reinterpret_cast<T*>(alloc(sizeof(T) * count, alignof(T)));
    }


    void* alloc(u32 bytes, u32 alignment)
    {
        if (bytes + alignment > current_page_remaining_) { // FIXME
            current_page_ = make_zeroed_sbr("xml");
            current_page_remaining_ = SCRATCH_BUFFER_SIZE;
            pages_.emplace_back(current_page_);
        }

        void* alloc_ptr = current_page_->data_ +
                          (SCRATCH_BUFFER_SIZE - current_page_remaining_);

        if (align(alignment, bytes, alloc_ptr, current_page_remaining_)) {
            current_page_remaining_ -= bytes;
            return alloc_ptr;
        }

        return nullptr;
    }
};



template <u8 pages> struct BulkAllocator
{
    BulkAllocator()
    {
        if (scratch_buffers_remaining() < pages) {
            warning("available scratch buffer count may be too low!");
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
