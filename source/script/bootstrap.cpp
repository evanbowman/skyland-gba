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


#include "memory/pool.hpp"
#include "memory/rc.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include <iostream>


// This file should contain the minimal subset of platform code necessary for
// running the interpreter.


u32 str_len(const char* str)
{
    const char* s;

    for (s = str; *s; ++s)
        ;
    return (s - str);
}


bool str_eq(const char* p1, const char* p2)
{
    while (true) {
        if (*p1 not_eq *p2) {
            return false;
        }
        if (*p1 == '\0' or *p2 == '\0') {
            return true;
        }
        ++p1;
        ++p2;
    }
}


int str_cmp(const char* p1, const char* p2)
{
    const unsigned char* s1 = (const unsigned char*)p1;
    const unsigned char* s2 = (const unsigned char*)p2;

    unsigned char c1, c2;

    do {
        c1 = (unsigned char)*s1++;
        c2 = (unsigned char)*s2++;

        if (c1 == '\0') {
            return c1 - c2;
        }

    } while (c1 == c2);

    return c1 - c2;
}


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


void str_reverse(char str[], int length)
{
    int start = 0;
    int end = length - 1;

    while (start < end) {
        std::swap(*(str + start), *(str + end));
        start++;
        end--;
    }
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


StringBuffer<12> stringify(s32 num)
{
    return to_string<12>(num);
}


void Platform::Logger::log(Severity level, const char* msg)
{
    std::cout << msg << std::endl;
}


void* Platform::system_call(const char* feature_name, void* arg)
{
    return nullptr;
}


bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    std::cout << text;
    std::cout << prompt;
    return true;
}


Platform::SystemClock::SystemClock()
{
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


std::optional<Function<4 * sizeof(void*), void()>> scratch_buffer_oom_handler;


void set_scratch_buffer_oom_handler(
    Function<4 * sizeof(void*), void()> callback)
{
    scratch_buffer_oom_handler.emplace(callback);
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
