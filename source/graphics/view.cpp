////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "view.hpp"


void View::set_center(const Vec2<Float>& center)
{
    center_ = center;
    int_center_.x = center.x;
    int_center_.y = center.y;
}


void View::set_size(const Vec2<Float>& size)
{
    size_ = size;
}


const Vec2<Float>& View::get_center() const
{
    return center_;
}


const Vec2<Float>& View::get_size() const
{
    return size_;
}
