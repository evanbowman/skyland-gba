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
#include "skyland/skyland.hpp"



namespace skyland
{



class CoreExplosionQuarter : public Entity
{
public:
    CoreExplosionQuarter(Platform::DynamicTexturePtr dt,
                         const Vec2<Float>& pos,
                         int quarter)
        : Entity({{}, {}}), dt_(dt), timer_(0), quarter_(quarter)
    {
        sprite_.set_texture_index(dt->mapping_index());

        auto p = pos;

        switch (quarter) {
        case 1:
            sprite_.set_flip({true, false});
            p.x += 32;
            break;

        case 2:
            sprite_.set_flip({false, true});
            p.y += 32;
            break;

        case 3:
            sprite_.set_flip({true, true});
            p.x += 32;
            p.y += 32;
            break;
        }

        sprite_.set_position(p);

        dt_->remap(70 * 2);
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);


            if (keyframe_ == 5) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_++;
            }
        }
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ += milliseconds(200);

            if (keyframe_ == 0) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_--;
            }
        }
    }


private:
    Platform::DynamicTexturePtr dt_;
    Microseconds timer_;
    int quarter_;
    int keyframe_ = 0;
};



inline void core_explosion(Platform& pfrm, App& app, const Vec2<Float>& pos)
{
    auto dt = pfrm.make_dynamic_texture();
    if (dt) {
        auto p = pos;
        p.x -= 32;
        p.y -= 32;
        auto make_segment = [&](int q) {
            return app.effects().push(
                app.alloc_entity<CoreExplosionQuarter>(pfrm, *dt, p, q));
        };
        make_segment(3);
        make_segment(2);
        make_segment(1);
        make_segment(0);
    }
}



} // namespace skyland
