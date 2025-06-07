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

#include "number/random.hpp"
#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class Ghost : public Entity
{
public:
    Ghost(const Vec2<Fixnum>& position) : Entity({})
    {
        sprite_.set_position(position);
        sprite_.set_texture_index(83 + rng::choice<3>(rng::utility_state));
        sprite_.set_size(Sprite::Size::w16_h32);
    }

    void update(Time delta) override
    {
        timer_ += delta;

        auto pos = sprite_.get_position();
        pos.y -= Fixnum(0.00004f) * APP.delta_fp();
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


    void rewind(Time delta) override
    {
        kill();
    }

private:
    Time timer_ = 0;
    int cycles_ = 0;
};



} // namespace skyland
