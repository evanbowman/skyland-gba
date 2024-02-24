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


Optional<Function<4 * sizeof(void*), void()>> scratch_buffer_oom_handler;


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
