////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "memory/pool.hpp"
#include "memory/rc.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include <chrono>
#include <iostream>


// This file should contain the minimal subset of platform code necessary for
// running the interpreter.


rng::Value rng::get(LinearGenerator& gen)
{
    gen = 1664525 * gen + 1013904223;
    return (gen >> 16) & 0x7FFF;
}


GenericPool* GenericPool::instances_;


void Platform::fatal(const char* msg)
{
    std::cerr << "fatal error: " << msg << std::endl;
    exit(EXIT_FAILURE);
}


Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    using namespace std::chrono;

    return time_point_cast<nanoseconds>(system_clock::now())
        .time_since_epoch()
        .count();
}


void arabic__to_string(int num, char* buffer, int base)
{
    int i = 0;
    bool is_negative = false;

    if (num == 0) {
        buffer[i++] = '0';
        buffer[i] = '\0';
        return;
    }

    // Based on the behavior of itoa()
    if (num < 0 && base == 10) {
        is_negative = true;
        num = -num;
    }

    while (num != 0) {
        int rem = num % base;
        buffer[i++] = (rem > 9) ? (rem - 10) + 'a' : rem + '0';
        num = num / base;
    }

    if (is_negative) {
        buffer[i++] = '-';
    }

    buffer[i] = '\0';

    str_reverse(buffer, i);

    return;
}


Platform* __platform__ = nullptr;


Platform::Platform()
{
    __platform__ = this;
}


Platform::~Platform()
{
}


template <u32 length> StringBuffer<length> to_string(int num)
{
    char temp[length];
    arabic__to_string(num, temp, 10);

    return temp;
}


void Platform::Logger::log(Severity level, const char* msg)
{
    std::cout << msg << std::endl;
}


const Platform::Extensions& Platform::get_extensions()
{
    static const Extensions ext{};
    return ext;
}



bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    std::cout << text;
    std::cout << prompt;
    return true;
}


Platform::NetworkPeer::NetworkPeer()
{
}


Platform::DeltaClock::DeltaClock()
{
}


Platform::Screen::Screen()
{
}


Platform::Speaker::Speaker()
{
}


Platform::Logger::Logger()
{
}


Platform::DeltaClock::~DeltaClock()
{
}


Platform::NetworkPeer::~NetworkPeer()
{
}


void Platform::sleep(u32)
{
}


static ObjectPool<PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>,
                  scratch_buffer_count>
    scratch_buffer_pool("scratch-buffers");


static int scratch_buffers_in_use_ = 0;


int scratch_buffers_remaining()
{
    return scratch_buffer_count - scratch_buffers_in_use_;
}


int scratch_buffers_in_use()
{
    return scratch_buffers_in_use_;
}


Optional<Function<4 * sizeof(void*), void()>> scratch_buffer_oom_handler;


void set_scratch_buffer_oom_handler(
    Function<4 * sizeof(void*), void()> callback)
{
    scratch_buffer_oom_handler.emplace(callback);
}


ScratchBufferPtr make_zeroed_sbr(const ScratchBuffer::Tag& tag,
                                 u32 zero_fill_size)
{
    auto sbr = make_scratch_buffer(tag);

    if (zero_fill_size > SCRATCH_BUFFER_SIZE) {
        PLATFORM.fatal("buffer overfill");
    }

    static const int wordsize = sizeof(void*);

    memset(sbr->data_, 0, zero_fill_size);

    return sbr;
}


ScratchBufferPtr make_scratch_buffer(const ScratchBuffer::Tag& tag)
{
    if (not scratch_buffers_remaining()) {
        if (scratch_buffer_oom_handler) {
            (*scratch_buffer_oom_handler)();

            if (not scratch_buffers_remaining()) {
                // Platform::instance().logger().clear();
            }
        }
    }

    auto finalizer =
        [](PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>* ctrl) {
            --scratch_buffers_in_use_;
            ctrl->pool_->free(ctrl);
        };

    auto maybe_buffer = create_pooled_rc<ScratchBuffer, scratch_buffer_count>(
        &scratch_buffer_pool, finalizer);
    if (maybe_buffer) {
        ++scratch_buffers_in_use_;

        (*maybe_buffer)->tag_ = tag;

        return *maybe_buffer;
    } else {
        Platform::fatal("scratch buffer pool exhausted");
    }
}
