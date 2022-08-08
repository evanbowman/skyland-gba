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


#pragma once

#include "number/endian.hpp"
#include "number/numeric.hpp"


class TimeTracker
{
public:
    TimeTracker(u32 secs) : time_(seconds(secs))
    {
    }

    u32 whole_seconds() const
    {
        return time_.get() / seconds(1);
    }

    void count_up(Microseconds delta)
    {
        if (delta < 0) {
            return;
        }
        time_.set(time_.get() + delta);
    }

    void count_down(Microseconds delta)
    {
        if ((u32)delta < time_.get()) {
            time_.set(time_.get() - delta);
        } else {
            time_.set(0);
        }
    }


    u64 total() const
    {
        return time_.get();
    }


    void reset(u32 secs)
    {
        time_.set(seconds(secs));
    }

private:
    HostInteger<u64> time_;
};
