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



std::optional<Function<16, void()>> scratch_buffer_oom_handler;



void set_scratch_buffer_oom_handler(Function<16, void()> callback)
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
            ctrl->pool_->post(ctrl);
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

    for (auto& cell : scratch_buffer_pool.cells()) {
        if (not scratch_buffer_pool.is_freed(&cell)) {
            *output += stringify(buffer_num);
            *output += ":";
            *output += ((ScratchBuffer*)cell.mem_.data())->tag_;
            *output += "\r\n";
            ++buffer_num;
        }
    }

    const int free_sbr = scratch_buffer_count - buffer_num;

    *output += format("used: % (%kb), free: % (%kb)\r\n",
                      buffer_num,
                      buffer_num * 2,
                      free_sbr,
                      free_sbr * 2)
                   .c_str();

    pfrm.remote_console().printline(output->c_str());
}
