#include "memory/pool.hpp"
#include "memory/rc.hpp"
#include "platform/platform.hpp"
#include <iostream>
#include "number/random.hpp"


// This file should contain the minimal subset of platform code necessary for
// running the interpreter.


static int scratch_buffers_in_use = 0;


rng::Value rng::get(LinearGenerator& gen)
{
    gen = 1664525 * gen + 1013904223;
    return (gen >> 16) & 0x7FFF;
}


ObjectPool<PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>,
               scratch_buffer_count>
        scratch_buffer_pool;


ScratchBufferPtr Platform::make_scratch_buffer()
{
    auto finalizer =
        [](PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>* ctrl) {
            --scratch_buffers_in_use;
            ctrl->pool_->post(ctrl);
        };

    auto maybe_buffer = create_pooled_rc<ScratchBuffer, scratch_buffer_count>(
        &scratch_buffer_pool, finalizer);
    if (maybe_buffer) {
        ++scratch_buffers_in_use;
        return *maybe_buffer;
    } else {
        // screen().fade(1.f, ColorConstant::electric_blue);
        fatal("scratch buffer pool exhausted");
    }
}


void Platform::fatal(const char* msg)
{
    std::cerr << "fatal error: " << msg << std::endl;
    exit(EXIT_FAILURE);
}


int Platform::scratch_buffers_remaining()
{
    return scratch_buffer_count - scratch_buffers_in_use;
}


void Platform::feed_watchdog()
{
}

void english__to_string(int num, char* buffer, int base)
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


Platform::Platform()
{
}


Platform::~Platform()
{
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


bool Platform::RemoteConsole::printline(const char* text, bool show_prompt)
{
    std::cout << text;
    return true;
}


void Platform::sleep(u32)
{
}
