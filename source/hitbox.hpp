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


#include "number/fixnum.hpp"
#include "number/numeric.hpp"



struct HitBox
{
    Vec2<Fixnum>* position_;

    struct Dimension
    {
        Vec2<s16> size_;
        Vec2<s16> origin_;
    } dimension_;

    bool overlapping(const HitBox& other) const
    {
        const auto c = center();
        const auto oc = other.center();
        if (c.x < (oc.x + other.dimension_.size_.x) and
            (c.x + dimension_.size_.x) > oc.x and
            c.y < (oc.y + other.dimension_.size_.y) and
            (c.y + dimension_.size_.y) > oc.y) {
            return true;
        } else {
            return false;
        }
    }

    Vec2<s16> center() const
    {
        Vec2<s16> c;
        c.x = s16(position_->x.as_integer()) - dimension_.origin_.x;
        c.y = s16(position_->y.as_integer()) - dimension_.origin_.y;
        return c;
    }
};
