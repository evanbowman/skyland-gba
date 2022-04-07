////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "skyland/entity.hpp"



namespace skyland
{



class Explosion : public Entity
{
public:
    static const int start_index = 19;


    Explosion(const Vec2<Float>& position) : Entity({{}, {}})
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(start_index);
        sprite_.set_origin({8, 8});
    }


    void update(Platform&, App&, Microseconds delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(55)) {
            timer_ = 0;

            auto index = sprite_.get_texture_index();
            if (index < start_index + 5) {
                sprite_.set_texture_index(index + 1);
            } else {
                kill();
            }
        }
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ = milliseconds(55);

            auto index = sprite_.get_texture_index();
            if (index > start_index) {
                sprite_.set_texture_index(index - 1);
            } else {
                kill();
            }
        }
    }


    void seek_end()
    {
        timer_ = milliseconds(55);
        sprite_.set_texture_index(start_index + 5);
    }


private:
    Microseconds timer_ = 0;
};



void medium_explosion(Platform& pfrm, App& app, const Vec2<Float>& position);
void medium_explosion_inv(Platform& pfrm,
                          App& app,
                          const Vec2<Float>& position);


void big_explosion(Platform& pfrm, App& app, const Vec2<Float>& position);
void big_explosion_inv(Platform& pfrm, App& app, const Vec2<Float>& position);



} // namespace skyland
