////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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

    void update(Microseconds delta) override
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


    void rewind(Microseconds delta) override
    {
        kill();
    }

private:
    Microseconds timer_ = 0;
    int cycles_ = 0;
};



} // namespace skyland
