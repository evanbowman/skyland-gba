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

#include "allocator.hpp"
#include "memory/buffer.hpp"
#include "number/endian.hpp"
#include "number/numeric.hpp"
#include "timeStreamHeader.hpp"
#include "timeTracker.hpp"



// Implements a generational queue of events in time. Used to implement rewind
// functionality in SKYLAND.



namespace skyland::time_stream
{



struct TimeBuffer
{

    u64 time_window_begin_;
    Time elapsed_ = 0;


    TimeBuffer(TimeTracker& begin) : time_window_begin_(begin.total())
    {
    }


    void update(Time delta)
    {
        elapsed_ += delta;
    }


    void rewind(Time delta)
    {
        elapsed_ -= delta;
    }


    char data_[1960];
    char* end_ = data_ + sizeof data_;


    template <typename T> bool push(T& elem)
    {
        static_assert(std::is_standard_layout<T>());
        static_assert(std::is_trivial<T>());
        static_assert(alignof(T) == 1);

        elem.header_.type_ = T::t;
        elem.header_.timestamp_.set(elapsed_);

        // NOTE: we allocate at higher addresses, and decrement the end_
        // pointer. As all elements will be cast by the caller from a Header*,
        // it's most convenient this way.

        if (end_ - sizeof elem >= data_) {
            auto alloc = end_ - sizeof elem;
            memcpy(alloc, &elem, sizeof elem);
            end_ = alloc;
            return true;
        } else {
            return false;
        }
    }


    void pop(u32 bytes)
    {
        if (end_ + bytes <= data_ + sizeof data_) {
            end_ += bytes;
        }
    }


    event::Header* end()
    {
        if (end_ == data_ + sizeof data_) {
            return nullptr;
        }
        return reinterpret_cast<event::Header*>(end_);
    }


    Optional<DynamicMemory<TimeBuffer>> next_;
};



class TimeStream
{
public:
    static const auto max_buffers = 28;


    template <typename T> void push(TimeTracker& current, T& event)
    {
        if (not enabled_pushes_) {
            return;
        }

        if (not buffers_) {
            buffers_ = allocate_dynamic<TimeBuffer>("time-stream", current);
            ++buffer_count_;
            end_ = &**buffers_;
        }

        while (not end_->push(event)) {
            end_->next_ = allocate_dynamic<TimeBuffer>("time-stream", current);
            end_ = &**end_->next_;
            ++buffer_count_;
        }

        if (buffer_count_ == max_buffers) {
            if (not buffers_ or not(*buffers_)->next_) {
                Platform::fatal("timestream logic error!");
            }
            // Unlink the first element in the chain, thus dropping the oldest
            // history.
            free_single_buffer();
        }
    }


    void free_single_buffer()
    {
        if (buffers_ and (*buffers_)->next_) {
            --buffer_count_;
            buffers_ = std::move(*(*buffers_)->next_);
        }
    }


    bool has_multiple_buffers() const
    {
        return buffers_ and (*buffers_)->next_;
    }


    event::Header* end();


    u64 begin_timestamp();


    Optional<u64> end_timestamp();


    // NOTE: this function rolls back elapsed_ time on the end_ block to that of
    // the timestamp of the popped event. This function is not intended to be
    // called unless you're actually rewinding time.
    void pop(u32 bytes);


    void update(Time delta);


    void rewind(Time delta);


    void clear();


    void enable_pushes(bool enabled)
    {
        enabled_pushes_ = enabled;
    }


    bool pushes_enabled() const
    {
        return enabled_pushes_;
    }


private:
    Optional<DynamicMemory<TimeBuffer>> buffers_;
    TimeBuffer* end_ = nullptr;
    u8 buffer_count_ = 0;
    bool enabled_pushes_ = false;
};



} // namespace skyland::time_stream
