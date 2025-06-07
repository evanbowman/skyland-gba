////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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
