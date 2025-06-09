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


#include "allocator.hpp"



// A Vector implementation backed by ScratchBuffers.

// For many types of data, SKYLAND allocates objects from pools. Consequently,
// we rarely have a reason to perform large-sized allocations. For general
// purpose allocations, our program allocates memory only as reference-counted
// 2k scratch buffers. We have a realtime program that runs on an embedded
// system with limited ram, and we want to avoid malloc. Here, we provide an
// implementation of a Dynamic Array compatible with our atypical memory
// allocators. No, this cannot be implemented instead as an allocator for
// std::vector, because we are unable to allocate large contiguous blocks of
// memory.
//
// Vector<> maintains a list of scratch buffers, each containing an array of
// elements. The class performs caching to make pushes to the end more
// efficient.
//
// I originally implemented Vector when I needed a datastructure capable of
// holding a lot of bytes for storing script data while implementing a text
// editor.
//
// Other features, such as logging, also leverage the Vector class.


template <typename T> class Vector
{
private:
    struct Chunk
    {
        struct Header
        {
            Optional<ScratchBufferPtr> next_;
            Chunk* prev_;
        };

        static constexpr u32 elems()
        {
            return ((SCRATCH_BUFFER_SIZE - sizeof(Chunk::Header)) -
                    alignof(T)) /
                   sizeof(T);
        }


        static void initialize(ScratchBufferPtr source, Chunk* prev)
        {
            new (source->data_) Chunk();
            ((Chunk*)source->data_)->header_.prev_ = prev;
        }


        Chunk() = default;


        Chunk(const Chunk& other) = delete;


        ~Chunk()
        {
            if (header_.next_) {
                ((Chunk*)(*header_.next_)->data_)->~Chunk();
            }
        }


        T* array()
        {
            return (T*)buffer_;
        }


        Header header_;
        alignas(T) char buffer_[elems() * sizeof(T)];
    };


    void seek_chunk(Chunk*& current, int& index) const
    {
        while ((u32)index >= Chunk::elems()) {
            if (current->header_.next_) {
                current = (Chunk*)(*current->header_.next_)->data_;
                index -= Chunk::elems();
            } else {
                return;
            }
        }
    }


public:
    struct Iterator
    {
        Iterator(int index, Chunk* chunk)
            : chunk_(chunk), index_(index), chunk_index_(index % Chunk::elems())
        {
        }


        bool operator==(const Iterator& other) const
        {
            return index_ == other.index_;
        }


        bool operator<(const Iterator& other) const
        {
            return index_ < other.index_;
        }


        bool operator>(const Iterator& other) const
        {
            return index_ > other.index_;
        }


        bool operator>=(const Iterator& other) const
        {
            return index_ >= other.index_;
        }


        bool operator<=(const Iterator& other) const
        {
            return index_ <= other.index_;
        }


        int operator-(const Iterator& other) const
        {
            return index_ - other.index_;
        }


        bool operator not_eq(const Iterator& other) const
        {
            return index_ not_eq other.index_;
        }


        T* operator->()
        {
            return &((chunk_->array())[chunk_index_]);
        }


        T& operator*()
        {
            return (chunk_->array())[chunk_index_];
        }


        Iterator operator++(int)
        {
            Iterator temp = *this;
            ++(*this);
            return temp;
        }


        const Iterator& operator++()
        {
            ++chunk_index_;
            ++index_;

            if (chunk_index_ == (int)Chunk::elems()) {
                chunk_index_ = 0;
                if (chunk_->header_.next_) {
                    chunk_ = (Chunk*)(*chunk_->header_.next_)->data_;
                }
            }

            return *this;
        }


        const Iterator& operator--()
        {
            --chunk_index_;
            --index_;

            if (index_ == 0) {
                chunk_index_ = 0;
                index_ = 0;
                return *this;
            }

            if (chunk_index_ < 0) {
                chunk_index_ = Chunk::elems() - 1;
                chunk_ = chunk_->header_.prev_;
            }

            return *this;
        }


        int index() const
        {
            return index_;
        }


    private:
        Chunk* chunk_;
        int index_;
        int chunk_index_;
    };


    Iterator begin()
    {
        return Iterator(0, (Chunk*)data_->data_);
    }


    Iterator begin() const
    {
        return Iterator(0, (Chunk*)data_->data_);
    }


    Iterator end() const
    {
        if (end_cache_) {
            return Iterator(size_, end_cache_);
        }

        int size = size_;
        Chunk* current = (Chunk*)data_->data_;
        seek_chunk(current, size);

        return Iterator(size_, current);
    }


    void erase(Iterator position)
    {
        auto last = end();

        for (; position not_eq last;) {
            auto next = position;
            ++next;

            // NOTE: we're copying the next element into the current
            // one. Because the last element does not have a next element, skip.
            if (position.index() not_eq last.index() - 1) {
                *position = std::move(*next);
            } else {
                // We copied the final element into the previous one in the
                // previous iteration. Simply destroy the final element.
                position->~T();
            }

            position = next;
        }
        --size_;
    }


    void insert(Iterator position, const T& elem)
    {
        end_cache_ = nullptr;

        push_back(elem);

        auto last = Iterator(size_ - 1, [this] {
            int index = size_ - 1;
            Chunk* chunk = (Chunk*)data_->data_;
            seek_chunk(chunk, index);
            return chunk;
        }());

        for (; position not_eq last; ++position) {
            std::swap(*position, *last);
        }
    }


    Vector(ScratchBuffer::Tag t = "") : data_(make_zeroed_sbr(t))
    {
        Chunk::initialize(data_, nullptr);
    }


    Vector(Vector&& other) : data_(other.data_), size_(other.size_)
    {
        other.valid_ = false;
    }


    Vector(const Vector& other) = delete;


    T& back()
    {
        auto last = end();
        --last;
        return *last;
    }


    void push_back(const T& elem, ScratchBuffer::Tag t = "")
    {
        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        if (end_cache_) {
            current = end_cache_;
            size = end_chunk_size_;
        } else {
            seek_chunk(current, size);
        }

        if (size == (int)Chunk::elems() and not current->header_.next_) {
            auto sbr = make_zeroed_sbr(t);
            Chunk::initialize(sbr, current);
            current->header_.next_ = sbr;
            current = (Chunk*)(*current->header_.next_)->data_;
            size = 0;
        }

        end_cache_ = current;
        end_chunk_size_ = size + 1;

        new (current->array() + size) T(elem);

        ++size_;
    }


    template <typename... Args> void emplace_back(Args&&... args)
    {
        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        if (end_cache_) {
            current = end_cache_;
            size = end_chunk_size_;
        } else {
            seek_chunk(current, size);
        }

        if (size == (int)Chunk::elems() and not current->header_.next_) {
            auto sbr = make_zeroed_sbr(data_->tag_);
            Chunk::initialize(sbr, current);
            current->header_.next_ = sbr;
            current = (Chunk*)(*current->header_.next_)->data_;
            size = 0;
        }

        end_cache_ = current;
        end_chunk_size_ = size + 1;

        new (current->array() + size) T(std::forward<Args>(args)...);

        ++size_;
    }


    void pop_back()
    {
        end_cache_ = nullptr;

        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        seek_chunk(current, size);

        --size_;
        (current->array() + (size - 1))->~T();
    }


    void shrink_to_fit()
    {
        if (size_ == 0) {
            Chunk* first = (Chunk*)data_->data_;
            if (first->header_.next_) {
                ((Chunk*)(*first->header_.next_)->data_)->~Chunk();
                first->header_.next_.reset();
            }
            end_cache_ = nullptr;
            return;
        }

        // Find the last chunk that actually contains data
        int elements_in_last_chunk = size_ % Chunk::elems();
        if (elements_in_last_chunk == 0) {
            elements_in_last_chunk = Chunk::elems();
        }

        int chunks_needed = (size_ + Chunk::elems() - 1) / Chunk::elems();

        // Walk to the last chunk we need
        Chunk* current = (Chunk*)data_->data_;
        for (int i = 1; i < chunks_needed; ++i) {
            if (current->header_.next_) {
                current = (Chunk*)(*current->header_.next_)->data_;
            } else {
                end_cache_ = nullptr;
                return;
            }
        }

        if (current->header_.next_) {
            ((Chunk*)(*current->header_.next_)->data_)->~Chunk();
            current->header_.next_.reset();
        }

        end_cache_ = nullptr;
    }


    T& operator[](int index)
    {
        Chunk* current = (Chunk*)data_->data_;

        seek_chunk(current, index);

        // TODO: bounds check!

        return current->array()[index];
    }


    ~Vector()
    {
        if (valid_) {
            if constexpr (not std::is_trivially_destructible<T>()) {
                clear();
            }
            ((Chunk*)data_->data_)->~Chunk();
        }
    }


    void clear()
    {
        while (size_) {
            pop_back();
        }
    }


    int chunks_used()
    {
        return size_ / Chunk::elems() + (size_ % Chunk::elems() > 0);
    }


    u32 size() const
    {
        return size_;
    }


private:
    ScratchBufferPtr data_;
    u32 size_ = 0;

    // Improves the performance of push_back() and end() and other operations
    // dealing with the end of the vector.
    mutable Chunk* end_cache_ = nullptr;
    mutable u16 end_chunk_size_ = 0;
    bool valid_ = true;
};
