////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "scratch_buffer.hpp"
#include "allocator.hpp"
#ifndef __TEST__
#include "platform.hpp"
#endif
#include "ext_workram_data.hpp"



static EXT_WORKRAM_DATA
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



ScratchBufferPtr make_zeroed_sbr(const ScratchBuffer::Tag& tag,
                                 u32 zero_fill_size)
{
    auto sbr = make_scratch_buffer(tag);

    if (zero_fill_size > SCRATCH_BUFFER_SIZE) {
        PLATFORM.fatal("buffer overfill");
    }

    static const int wordsize = sizeof(void*);

    if ((intptr_t)sbr->data_ % wordsize == 0 and
        zero_fill_size % wordsize == 0) {
        PLATFORM.memset_words(sbr->data_, 0, zero_fill_size / wordsize);
    } else {
        memset(sbr->data_, 0, zero_fill_size);
    }

    return sbr;
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



void scratch_buffer_memory_diagnostics()
{
    StringBuffer<96> output;

    int buffer_num = 0;
    int buffers_used = 0;

    info("scratch buffer (sbr) diagnostics: ");
    info("  # |  name");
    info("----|----------------------------");

    for (auto& cell : scratch_buffer_pool.cells()) {
        if (not scratch_buffer_pool.is_freed(&cell)) {
            output = " ";
            output += stringify(buffer_num);
            output += " | ";
            output += ((ScratchBuffer*)cell.mem_.data())->tag_;
            info(output);
            ++buffers_used;
        }
        ++buffer_num;
    }

    const int free_sbr = scratch_buffer_count - buffers_used;

    output = format("sbr used: % (%kb), free: % (%kb)",
                    buffers_used,
                    buffers_used * 2,
                    free_sbr,
                    free_sbr * 2)
                 .c_str();

    info(output);
}



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
        PLATFORM_EXTENSION(feed_watchdog);
    }

    StringBuffer<50> complete("dumped : ");
    complete += ((ScratchBuffer*)page.mem_.data())->tag_;

    PLATFORM.remote_console().printline(complete.c_str(), "sc> ");
}
#endif
