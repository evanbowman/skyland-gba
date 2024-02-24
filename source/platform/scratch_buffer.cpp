
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


#include "scratch_buffer.hpp"
#include "allocator.hpp"
#ifndef __TEST__
#include "platform.hpp"
#endif
#include "heap_data.hpp"



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
#ifndef __TEST__
                Platform::instance().logger().clear();
#endif
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



#ifndef __TEST__
void scratch_buffer_memory_diagnostics()
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

    PLATFORM.remote_console().printline(output->c_str(), "sc> ");
}
#endif


#ifndef __TEST__
void scratch_buffer_dump_sector(int sector)
{
    if (sector >= (int)scratch_buffer_pool.cells().size()) {
        return;
    }

    static_assert(SCRATCH_BUFFER_SIZE % 32 == 0);

    auto page = scratch_buffer_pool.cells()[sector];

    const u8* p = page.mem_.data();


    for (int row = 0; row < SCRATCH_BUFFER_SIZE / 32; ++row) {
        StringBuffer<200> out;
        for (int i = 0; i < 32; ++i) {
            const char* hex = "0123456789ABCDEF";

            out.push_back(hex[(*p & 0xf0) >> 4]);
            out.push_back(hex[(*p & 0x0f)]);
            out.push_back(' ');

            ++p;
        }

        PLATFORM.remote_console().printline(out.c_str(), "");
        PLATFORM.sleep(20);
        PLATFORM.system_call("feed-watchdog", nullptr);
    }

    StringBuffer<50> complete("dumped : ");
    complete += ((ScratchBuffer*)page.mem_.data())->tag_;

    PLATFORM.remote_console().printline(complete.c_str(), "sc> ");
}
#endif
