////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "ballistaBolt.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



extern Sound sound_impact;
SHARED_VARIABLE(ballista_damage);
SHARED_VARIABLE(ballista_splash_damage);



void BallistaBolt::generate_path(Path& path,
                                 Fixnum start_x,
                                 Fixnum start_y,
                                 Fixnum target_x,
                                 Fixnum target_y,
                                 Fixnum arc_height)
{
    int steps = path.capacity() - 1;

    path.clear();

    // Pre-computed values for 8 steps
    static_assert(path.capacity() - 1 == 8);
    static constexpr Fixnum t_values[8] = {
        0.0_fixed,   // 0/8
        0.125_fixed, // 1/8
        0.25_fixed,  // 2/8
        0.375_fixed, // 3/8
        0.5_fixed,   // 4/8
        0.625_fixed, // 5/8
        0.75_fixed,  // 6/8
        0.875_fixed  // 7/8
    };

    for (int i = 0; i < steps; ++i) {
        auto t = t_values[i];

        Fixnum x = start_x + (target_x - start_x) * t;

        Fixnum parabolic_factor =
            4.0_fixed * t * (1.0_fixed - t); // Peaks at t=0.5
        Fixnum y =
            start_y + (target_y - start_y) * t - arc_height * parabolic_factor;

        path.push_back({(s16)x.as_integer(), (s16)y.as_integer()});
    }

    path.push_back({(s16)target_x.as_integer(), (s16)target_y.as_integer()});
}



BallistaBolt::BallistaBolt(const Vec2<Fixnum>& position,
                           const Vec2<Fixnum>& target,
                           Fixnum arc_height,
                           Island& src)
    : Projectile({{8, 8}, {8, 8}})
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_tidx_16x16(80, 1);

    sprite_.set_origin({8, 8});

    generate_path(
        state_.path_, position.x, position.y, target.x, target.y, arc_height);

    state_.interp_ms_ = (arc_height.as_integer() +
                         abs(state_.path_.back().x - state_.path_[0].x)) /
                        16;

    state_.player_src_ = is_player_island(&src);
}



void BallistaBolt::update(Time delta)
{
    state_.timer_ += delta;

    auto current = state_.path_[state_.path_idx_];
    auto next = state_.path_[std::min(state_.path_idx_ + 1,
                                      (int)state_.path_.size() - 1)];

    auto wc = [](Vec2<s16> p) {
        return Vec2<Fixnum>{Fixnum::from_integer(p.x),
                            Fixnum::from_integer(p.y)};
    };

    auto move_rate = milliseconds(state_.interp_ms_ * 10);

    Fixnum interval(Float(state_.timer_) / move_rate);

    auto point = interpolate_fp(wc(next), wc(current), interval);
    sprite_.set_position(point);

    if (state_.timer_ > move_rate) {
        state_.timer_ -= move_rate;

        state_.path_idx_++;

        if (state_.path_idx_ == state_.path_.size()) {
            destroy();
            return;
        }
    }
}



void BallistaBolt::rewind(Time delta)
{
    state_.timer_ -= delta;

    auto move_rate = milliseconds(state_.interp_ms_ * 10);

    if (state_.timer_ <= 0) {
        state_.timer_ += move_rate;

        if (state_.path_idx_ == 0) {
            kill();
            return;
        }

        state_.path_idx_--;

        auto wc = [](Vec2<s16> p) {
            return Vec2<Fixnum>{Fixnum::from_integer(p.x),
                                Fixnum::from_integer(p.y)};
        };

        sprite_.set_position(wc(state_.path_[state_.path_idx_]));
    }
}



void BallistaBolt::on_collision(Room& room, Vec2<u8> origin)
{
    Health damage = ballista_damage;

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() <= damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    auto src = state_.player_src_ ? &player_island() : opponent_island();

    if (src and src == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    int grid_x_start = room.position().x;
    int grid_y_start = room.position().y;

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto r = island->get_room({u8(x), u8(y)})) {
                r->apply_damage(damage);
            }
        }
    };

    apply_damage(0, 0, damage);
    apply_damage(1, 0, ballista_splash_damage);
    apply_damage(-1, 0, ballista_splash_damage);
    apply_damage(0, 1, ballista_splash_damage);
    apply_damage(0, -1, ballista_splash_damage);

    if (str_eq(room.name(), "mirror-hull")) {
        destroy();
        // TODO!!!
        PLATFORM.speaker().play_sound("cling", 2);
    } else {
        if (room.health()) {
            sound_impact.play(1);
        }
        destroy();
    }
}



void BallistaBolt::on_collision(Entity& entity)
{
    bool skip_destroy = false;

    if (auto drone = entity.cast_drone()) {
        if (drone->ignores_damage()) {
            skip_destroy = true;
        }
    }


    if (not skip_destroy) {
        this->destroy();
    }

    auto damage = (int)ballista_damage;

    entity.apply_damage(damage);
}



void BallistaBolt::destroy()
{
    APP.camera()->shake(8);
    big_explosion(sprite_.get_position(),
                  {
                      .centerflash_ = true,
                  });
    kill();

    time_stream::event::PlayerBallistaBoltDestroyed e;
    e.x_pos_.set(sprite_.get_position().x.as_integer());
    e.y_pos_.set(sprite_.get_position().y.as_integer());
    memcpy(e.state_, &state_, sizeof(State));
    APP.time_stream().push(APP.level_timer(), e);
}



BallistaBolt::BallistaBolt(const Vec2<Fixnum>& pos, const State& s)
    : Projectile({{10, 10}, {8, 8}})
{
    sprite_.set_position(pos);
    state_ = s;

    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(18 * 2);
    sprite_.set_tidx_16x16(80, 1);

    sprite_.set_origin({8, 8});
}



} // namespace skyland
