////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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
