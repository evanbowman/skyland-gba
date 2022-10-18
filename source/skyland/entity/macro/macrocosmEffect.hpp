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


#include "macrocosmEntity.hpp"



namespace skyland::macro
{



class MacrocosmEffect : public MacrocosmEntity
{
public:
    MacrocosmEffect(const Vec2<Fixnum>& position,
                    int begin_tile,
                    int end_tile,
                    Microseconds rate)
        : rate_(rate), begin_tile_(begin_tile), end_tile_(end_tile)
    {
        sprite_.set_size(Sprite::Size::w32_h32);
        sprite_.set_texture_index(begin_tile);
        sprite_.set_position(position);
        sprite_.set_origin({8, 8});
    }


    void update(Platform& pfrm, macro::Engine& s, Microseconds delta) override
    {
        timer_ += delta;
        if (timer_ >= rate_) {
            timer_ -= rate_;

            auto t = sprite_.get_texture_index();
            if (t == end_tile_) {
                kill();
                return;
            }

            ++t;
            sprite_.set_texture_index(t);
        }
    }


private:
    Microseconds timer_ = 0;
    Microseconds rate_;
    u16 begin_tile_;
    u16 end_tile_;
};



} // namespace skyland::macro
