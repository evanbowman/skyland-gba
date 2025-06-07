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


class View
{
public:
    void set_center(const Vec2<Float>& center);

    void set_size(const Vec2<Float>& size);

    const Vec2<Float>& get_center() const;
    const Vec2<int>& int_center() const
    {
        return int_center_;
    }


    const Vec2<Float>& get_size() const;

private:
    Vec2<Float> center_;
    Vec2<Float> size_;
    Vec2<int> int_center_;
};
