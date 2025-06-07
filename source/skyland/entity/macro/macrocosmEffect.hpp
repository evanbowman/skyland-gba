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


#include "macrocosmEntity.hpp"



namespace skyland::macro
{



class MacrocosmEffect : public MacrocosmEntity
{
public:
    MacrocosmEffect(const Vec2<Fixnum>& position,
                    int begin_tile,
                    int end_tile,
                    Time rate)
        : rate_(rate), begin_tile_(begin_tile), end_tile_(end_tile)
    {
        sprite_.set_size(Sprite::Size::w32_h32);
        sprite_.set_texture_index(begin_tile_);
        sprite_.set_position(position);
        sprite_.set_origin({8, 8});
    }


    void update(macro::Engine& s, Time delta) override
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
    Time timer_ = 0;
    Time rate_;
    u16 begin_tile_;
    u16 end_tile_;
};



} // namespace skyland::macro
