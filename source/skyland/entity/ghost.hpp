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

#include "number/random.hpp"
#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class Ghost : public Entity
{
public:
    Ghost(const Vec2<Fixnum>& position) :
        Entity({})
    {
        sprite_.set_position(position);
        sprite_.set_texture_index(83 + rng::choice<3>(rng::utility_state));
        sprite_.set_size(Sprite::Size::w16_h32);
    }

    void update(Platform&, App& app, Microseconds delta) override
    {
        timer_ += delta;

        auto pos = sprite_.get_position();
        pos.y -= Fixnum(0.00004f) * app.delta_fp();
        sprite_.set_position(pos);

        if (timer_ > milliseconds(150)) {
            timer_ = 0;

            ++cycles_;

            auto index = sprite_.get_texture_index();
            if (index < 83 + 2) {
                sprite_.set_texture_index(index + 1);
            } else {
                sprite_.set_texture_index(83);
            }

            if (cycles_ > 15) {
                kill();
            }
            if (cycles_ > 10) {
                sprite_.set_alpha(Sprite::Alpha::translucent);
            }
        }
    }


    bool entity_oom_deletable() const override
    {
        return false;
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        kill();
    }

private:
    Microseconds timer_ = 0;
    int cycles_ = 0;
};



}
