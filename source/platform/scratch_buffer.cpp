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


#include "scratch_buffer.hpp"
#include "allocator.hpp"
#include "platform.hpp"

#if defined(__NDS__)
#define HEAP_DATA
#elif defined(__GBA__)
#define HEAP_DATA __attribute__((section(".ewram")))
#else
#define HEAP_DATA
#endif



static HEAP_DATA
    ObjectPool<PooledRcControlBlock<ScratchBuffer, scratch_buffer_count>,
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
                Platform::instance().logger().clear();
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



void scratch_buffer_memory_diagnostics(Platform& pfrm)
{
    auto output = allocate_dynamic<Platform::RemoteConsole::Line>(
        "sbr-annotation-buffer");

    int buffer_num = 0;
    int buffers_used = 0;

    for (auto& cell : scratch_buffer_pool.cells()) {
        if (not scratch_buffer_pool.is_freed(&cell)) {
            *output += "@";
            *output += stringify(buffer_num);
            *output += ": ";
            *output += ((ScratchBuffer*)cell.mem_.data())->tag_;
            *output += "\r\n";
            ++buffers_used;
        }
        ++buffer_num;
    }

    const int free_sbr = scratch_buffer_count - buffers_used;

    *output += format("used: % (%kb), free: % (%kb)\r\n",
                      buffers_used,
                      buffers_used * 2,
                      free_sbr,
                      free_sbr * 2)
                   .c_str();

    pfrm.remote_console().printline(output->c_str(), "sc> ");
}



void scratch_buffer_dump_sector(Platform& pfrm, int sector)
{
    if (sector >= (int)scratch_buffer_pool.cells().size()) {
        return;
    }

    static_assert(SCRATCH_BUFFER_SIZE % 40 == 0);

    auto page = scratch_buffer_pool.cells()[sector];

    const u8* p = page.mem_.data();


    for (int row = 0; row < SCRATCH_BUFFER_SIZE / 40; ++row) {
        StringBuffer<200> out;
        for (int i = 0; i < 40; ++i) {
            const char* hex = "0123456789ABCDEF";

            out.push_back(hex[(*p & 0xf0) >> 4]);
            out.push_back(hex[(*p & 0x0f)]);
            out.push_back(' ');

            ++p;
        }

        pfrm.remote_console().printline(out.c_str(), "");
        pfrm.sleep(20);
        pfrm.system_call("feed-watchdog", nullptr);
    }

    StringBuffer<50> complete("dumped : ");
    complete += ((ScratchBuffer*)page.mem_.data())->tag_;

    pfrm.remote_console().printline(complete.c_str(), "sc> ");
}
