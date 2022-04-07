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


#include "number/int.h"


struct Date
{
    s32 year_;
    s32 month_;
    s32 day_;
};


struct DateTime
{
    Date date_;
    s32 hour_;
    s32 minute_;
    s32 second_;

    u64 as_seconds() const
    {
        auto minute = [](u64 n) { return n * 60; };
        auto hour = [&minute](u64 n) { return n * minute(60); };
        auto day = [&hour](u64 n) { return n * hour(24); };
        auto month = [&day](u64 n) { return n * day(31); }; // FIXME...
        auto year = [&day](u64 n) { return n * day(365); }; // FIXME...

        return second_ + minute(minute_) + hour(hour_) + day(date_.day_) +
               month(date_.month_) + year(date_.year_);
    }
};


inline u64 time_diff(const DateTime& before, const DateTime& after)
{
    return after.as_seconds() - before.as_seconds();
}
