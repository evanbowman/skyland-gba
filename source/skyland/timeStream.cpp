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


#include "timeStream.hpp"



namespace skyland::time_stream
{



void TimeStream::pop(u32 bytes)
{
    if (end_) {
        if (auto hdr = end_->end()) {
            // We're rolling back the times stream, let's make sure that we
            // update the highest timestamp in the block.
            end_->elapsed_ = hdr->timestamp_.get();
        }

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
                    if (buffer_count_ == 0) {
                        Platform::fatal(
                            "logic error: freed a timestream buffer,"
                            "but buffer count is zero!");
                    }
                    --buffer_count_;
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



void TimeStream::rewind(Microseconds delta)
{
    if (end_) {
        end_->rewind(delta);
        if (end_->elapsed_ < 0) {
            end_->elapsed_ = 0;
        }
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