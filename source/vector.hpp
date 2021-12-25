#pragma once


#include "bulkAllocator.hpp"



// A Vector implementation backed by ScratchBuffers.

// Our program allocates memory only as reference-counted 2k buffers. Here, we
// provide an implementation of a Dynamic Array compatible with our atypical
// memory allocators.



template <typename T>
class Vector {
private:

    struct Chunk {
        struct Header {
            std::optional<ScratchBufferPtr> next_;
        };

        static constexpr u32 elems()
        {
            return ((SCRATCH_BUFFER_SIZE - sizeof(Chunk::Header))
                    - alignof(T)) / sizeof(T);
        }


        static void initialize(ScratchBufferPtr source)
        {
            new (source->data_) Chunk();
        }


        ~Chunk()
        {
            if (header_.next_) {
                ((Chunk*)header_->next_->data_)->~Chunk();
            }
        }


        T* array()
        {
            return (T*)buffer_;
        }


        Header header_;
        alignas(T) char buffer_[elems() * sizeof(T)];
    };


    void seek_chunk(Chunk*& current, int& index)
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


    struct Iterator {
        Iterator(int index, Chunk* chunk) :
            chunk_(chunk),
            index_(index),
            chunk_index_(index % Chunk::elems())
        {
        }


        bool operator==(const Iterator& other) const
        {
            return index_ == other.index_;
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


        const Iterator& operator++()
        {
            ++chunk_index_;
            ++index_;

            if (chunk_index_ == Chunk::elems()) {
                chunk_index_ = 0;
                if (chunk_->header_.next_) {
                    chunk_ = (Chunk*)(*chunk_->header_.next_)->data_;
                }
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


    Iterator end()
    {
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
        // I implemented it this way because it was simply easiest, because with
        // our list of allocated chunks, reverse iteration would require some
        // changes. Store the newly inserted element at the end. For each
        // element, swap current with last. This effectively shifts the elements
        // to the right, using the final element as a placeholder, and using the
        // existing logic in push_back() to handle reallocations. Feels sort of
        // clever, although I can't be the first person who's thought of this.

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


    Vector(Platform& pfrm) :
        pfrm_(pfrm),
        data_(pfrm.make_scratch_buffer())
    {
        Chunk::initialize(data_);
    }


    void push_back(const T& elem)
    {
        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        seek_chunk(current, size);

        if (size == Chunk::elems() and not current->header_.next_) {
            auto sbr = pfrm_.make_scratch_buffer();
            Chunk::initialize(sbr);
            current->header_.next_ = sbr;
            current = (Chunk*)(*current->header_.next_)->data_;
            size = 0;
        }

        new (current->array() + size) T(elem);

        ++size_;
    }


    void pop_back()
    {
        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        seek_chunk(current, size);

        --size_;
        (current->array() + (size - 1))->~T();
        *(current->array() + (size - 1)) = 0; // TODO: Remove this line!
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
        while (size_ > Chunk::elems()) {
            // TODO... delete stuff.
        }
    }


    int chunks_used()
    {
        return size_ / Chunk::elems() + (size_ % Chunk::elems() > 0);
    }


private:
    Platform& pfrm_;
    ScratchBufferPtr data_;

    u32 size_ = 0;
};
