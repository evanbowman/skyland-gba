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

#include "explosion.hpp"
#include "explosion2.hpp"
#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class CoreExplosionQuarter : public Entity
{
public:
    CoreExplosionQuarter(Platform::DynamicTexturePtr dt,
                         const Vec2<Fixnum>& pos,
                         int quarter)
        : Entity({{}, {}}), dt_(dt), timer_(0), quarter_(quarter)
    {
        sprite_.set_texture_index(dt->mapping_index());

        auto p = pos;

        switch (quarter) {
        case 1:
            sprite_.set_flip({true, false});
            p.x += 32.0_fixed;
            break;

        case 2:
            sprite_.set_flip({false, true});
            p.y += 32.0_fixed;
            break;

        case 3:
            sprite_.set_flip({true, true});
            p.x += 32.0_fixed;
            p.y += 32.0_fixed;
            break;
        }

        sprite_.set_position(p);

        dt_->remap(70 * 2);
    }


    void update(App& app, Microseconds delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);


            if (keyframe_ == 5) {
                kill();
                if (quarter_ == 0) {
                    time_stream::event::CoreExplosion e;
                    const auto pos = sprite_.get_position();
                    e.x_.set(pos.x.as_integer());
                    e.y_.set(pos.y.as_integer());
                    app.time_stream().push(app.level_timer(), e);
                }
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_++;
            }
        }
    }


    void jump_to_end()
    {
        timer_ = milliseconds(200);
        keyframe_ = 4;
        dt_->remap((70 + keyframe_) * 2);
    }


    void rewind(App& app, Microseconds delta) override
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



inline void core_explosion(App& app, Island* parent, const Vec2<Fixnum>& pos)
{
    if (parent->core_count() == 1) {
        // There's a special death sequence animation for the final destroyed
        // core.
        big_explosion(app, pos);
        return;
    }


    int min_x = PLATFORM.screen().get_view().int_center().x - 48;
    int max_x = PLATFORM.screen().get_view().int_center().x +
                PLATFORM.screen().size().x + 48;
    int max_y = 700;
    int min_y = 450;

    if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
        pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
        // Don't create the explosion effect if way outside of the camera range.
    } else {
        app.effects().clear();

        for (int i = 0; i < 8; ++i) {
            for (int j = 0; j < 4; ++j) {
                const int angle = j * 90 + 45 + i * 3;
                const u8 half_angle = angle / 2;
                if (auto exp =
                        app.alloc_entity<Explosion2>(pos, half_angle, (u8)i)) {
                    auto dir = rotate({1, 0}, angle);
                    dir = dir * (((i + 1 / 2.f) * 1.5f) * 0.00005f);
                    Vec2<Fixnum> spd;
                    spd.x = Fixnum(dir.x);
                    spd.y = Fixnum(dir.y);
                    exp->set_speed(spd);
                    app.effects().push(std::move(exp));
                }
            }
        }
    }


    auto dt = PLATFORM.make_dynamic_texture();
    if (dt) {
        auto p = pos;
        p.x -= 32.0_fixed;
        p.y -= 32.0_fixed;
        auto make_segment = [&](int q) {
            return app.effects().push(
                app.alloc_entity<CoreExplosionQuarter>(*dt, p, q));
        };
        make_segment(3);
        make_segment(2);
        make_segment(1);
        make_segment(0);
    }

    app.camera()->shake(28);
}



} // namespace skyland
