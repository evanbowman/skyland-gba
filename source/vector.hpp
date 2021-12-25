#pragma once


#include "bulkAllocator.hpp"



// A Vector implementation backed by ScratchBuffers.

// Our program allocates memory only as reference-counted 2k buffers. We have a
// realtime program that runs on an embedded system with limited ram, and we
// want to avoid malloc. Here, we provide an implementation of a Dynamic Array
// compatible with our atypical memory allocators. No, this cannot be
// implemented instead as an allocator for std::vector.



template <typename T>
class Vector {
private:

    struct Chunk {
        struct Header {
            std::optional<ScratchBufferPtr> next_;
            Chunk* prev_;
        };

        static constexpr u32 elems()
        {
            return ((SCRATCH_BUFFER_SIZE - sizeof(Chunk::Header))
                    - alignof(T)) / sizeof(T);
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

            if (chunk_index_ == Chunk::elems()) {
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
        Chunk::initialize(data_, nullptr);
    }


    Vector(Vector&& other) :
        pfrm_(other.pfrm_),
        data_(other.data_),
        size_(other.size_)
    {
        other.size_ = 0; // Should be sufficient to invalidate the other
                         // vector. It will hold onto its scratch buffer, which
                         // cannot be null, but it's just a reference count and
                         // will be decremented eventually.
    }


    Vector(const Vector& other) = delete;


    void push_back(const T& elem)
    {
        Chunk* current = (Chunk*)data_->data_;

        int size = size_;

        seek_chunk(current, size);

        if (size == Chunk::elems() and not current->header_.next_) {
            auto sbr = pfrm_.make_scratch_buffer();
            Chunk::initialize(sbr, current);
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
        // TODO: Don't bother to pop anything if elements are trivially
        // destructible.
        while (size_ > Chunk::elems()) {
            pop_back();
        }
        ((Chunk*)data_->data_)->~Chunk();
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
    Platform& pfrm_;
    ScratchBufferPtr data_;

    u32 size_ = 0;
};
