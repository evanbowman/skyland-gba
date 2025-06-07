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

#include "number/numeric.hpp"
#include "platform/platform.hpp"


class Rumble
{
public:
    void update(Microseconds dt)
    {
        if (duration_ > 0) {
            duration_ -= dt;
            if (duration_ <= 0) {
                duration_ = 0;
                enabled_ = false;
                PLATFORM.keyboard().rumble(false);
            }
        }
    }

    void activate(Microseconds duration)
    {
        if (not enabled_) {
            PLATFORM.keyboard().rumble(true);
        }
        duration_ = std::max(duration, duration_);
        enabled_ = true;
    }

private:
    bool enabled_ = false;
    Microseconds duration_ = 0;
};
