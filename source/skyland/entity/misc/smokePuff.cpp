////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "smokePuff.hpp"



namespace skyland
{



static u8 instance_count = 0;



SmokePuff::SmokePuff(const Vec2<Fixnum>& position, u16 tile) : Entity({{}, {}})
{
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(tile);
    sprite_.set_position(position);
    sprite_.set_alpha(Sprite::Alpha::translucent);
    sprite_.set_origin({8, 8});
    ++instance_count;
}



SmokePuff::~SmokePuff()
{
    --instance_count;
}



u8 SmokePuff::get_instance_count()
{
    return instance_count;
}



} // namespace skyland
