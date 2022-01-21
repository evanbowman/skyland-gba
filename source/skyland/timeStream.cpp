#include "timeStream.hpp"



namespace skyland::time_stream {



void TimeStream::pop(u32 bytes)
{
    if (end_) {
        end_->pop(bytes);

        if (end_->end() == nullptr) {
            TimeBuffer* last = nullptr;
            TimeBuffer* current = &**buffers_;
            while (current) {
                if (current == end_) {
                    end_ = last;
                    if (last) {
                        last->next_.reset();
                    } else {
                        buffers_.reset();
                    }
                    break;
                }
                last = current;
                if (not current->next_) {
                    Platform::fatal("logic error: if current != end, "
                                    "current must have next.");
                }
                current = &**current->next_;
            }
        }
    }
}



void TimeStream::update(Microseconds delta)
{
    if (end_) {
        end_->update(delta);
    }
}



void TimeStream::clear()
{
    buffers_.reset();
    end_ = nullptr;
    buffer_count_ = 0;
}



u64 TimeStream::begin_timestamp()
{
    if (buffers_) {
        return (*buffers_)->time_window_begin_;
    } else {
        return 0;
    }
}



event::Header* TimeStream::end()
{
    if (end_) {
        return end_->end();
    }
    return nullptr;
}



std::optional<u64> TimeStream::end_timestamp()
{
    if (end_) {
        if (end_->end()) {
            return end_->time_window_begin_ + end_->end()->timestamp_.get();
        }
    }
    return {};
}



} // namespace skyland::time_stream
