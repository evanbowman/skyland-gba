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


#include "drone.hpp"
#include "droneMeta.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/room.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static Vec2<Fixnum> calc_pos(Island* island, const Vec2<u8>& grid_coord)
{
    auto o = island->visual_origin();
    o.x += grid_coord.x * 16;
    o.y += grid_coord.y * 16;
    return o;
}



Drone::Drone(const char* name,
             Island* parent,
             Island* destination,
             const Vec2<u8>& grid_pos)
    : Entity({{16, 16}, {0, 0}}), parent_(parent), destination_(destination),
      grid_pos_({grid_pos.x, u8(grid_pos.y)})
{
    sprite_.set_texture_index(64);
    sprite_.set_size(Sprite::Size::w16_h32);

    health_ = 40;

    auto o = calc_pos(parent_, grid_pos);
    sprite_.set_position(o);
    anchor_ = Vec2<s16>{(s16)ivec(o).x, (s16)ivec(o).y};

    target_near_ = false;

    metaclass_index_ = DroneMeta::index(name);
}



void Drone::set_movement_target(const Vec2<u8>& position)
{
    auto old_pos = grid_pos_;
    grid_pos_ = position;

    auto v1 = calc_pos(parent_, old_pos);
    auto v2 = calc_pos(destination_, grid_pos_);

    auto dist = distance(fvec(v1), fvec(v2));

    auto speed = 0.0002f;

    duration_ = dist / speed;
}



void Drone::update_sprite(Platform& pfrm, App& app)
{
    auto o = calc_pos(destination_, grid_pos_);

    Float offset = 3 * float(sine(4 * 3.14f * 0.0005f * duration_ + 180)) /
                   std::numeric_limits<s16>::max();

    if (pfrm.network_peer().is_connected()) {
        // The floating movement complicates proper collision checking, as
        // minute differences in the game clocks in multiplayer mode can result
        // in different collision results in each game, if the drone moves
        // subtly up and down.
        offset = 0;
    }

    o.y += offset;

    sprite_.set_position(o);

    if (target_) {
        if (destination() == app.opponent_island() and target_near_) {
            sprite_.set_flip({true, false});
        } else if (destination() == &app.player_island() and not target_near_) {
            sprite_.set_flip({false, false});
        } else {
            if (target_->x < grid_pos_.x) {
                sprite_.set_flip({true, false});
            } else {
                sprite_.set_flip({false, false});
            }
        }
    }
}



void Drone::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    switch (state_) {
    case State::launch: {
        timer_ -= delta;
        if (timer_ < 0) {
            kill();
            // TODO: reset timer of parent drone-bay.
        } else {
            auto amount = smoothstep(0.f, duration_, timer_);
            auto dest = calc_pos(destination_, grid_pos_);
            auto pos =
                interpolate(fvec(dest),
                            Vec2<Float>{(Float)anchor_.x,
                                        (Float)anchor_.y},
                            Float(amount));

            sprite_.set_position(Vec2<Fixnum>{pos.x, pos.y});

            if (parent() not_eq &app.player_island()) {
                sprite_.set_flip({true, false});
            }
        }
        break;
    }

    default:
        update_sprite(pfrm, app);
        if (timer_ > 0) {
            timer_ -= delta;
        }
        break;
    }
}



void Drone::set_target(Platform& pfrm,
                       App& app,
                       const Vec2<u8>& target,
                       bool target_near)
{
    if (target_) {
        if (*target_ == target and target_near_ == target_near) {
            // Optimization to save space in the rewind buffer.
            return;
        }
    }

    time_stream::event::DroneSetTarget e;
    e.x_pos_ = grid_pos_.x;
    e.y_pos_ = grid_pos_.y;
    if (target_) {
        e.has_previous_target_ = true;
        e.previous_target_x_ = target_->x;
        e.previous_target_y_ = target_->y;
    } else {
        e.has_previous_target_ = false;
    }
    e.previous_target_near_ = target_near_;
    e.destination_near_ = destination_ == &app.player_island();
    app.time_stream().push(app.level_timer(), e);

    target_ = target;
    target_near_ = target_near;
}



void Drone::drop_target(Platform& pfrm, App& app)
{
    if (app.time_stream().pushes_enabled()) {
        Platform::fatal("drop_target intended to be called only during rewind, "
                        "as we do not push a time stream event in this message "
                        "for rolling back a drop_target().");
    }

    target_.reset();
}



void Drone::apply_damage(Platform& pfrm, App& app, Health amount)
{
    time_stream::event::DroneHealthChanged e;
    e.x_pos_ = grid_pos_.x;
    e.y_pos_ = grid_pos_.y;
    e.destination_near_ = destination_ == &app.player_island();
    e.previous_health_.set(health());
    app.time_stream().push(app.level_timer(), e);

    Entity::apply_damage(pfrm, app, amount);
}



void Drone::update(Platform& pfrm, App& app, Microseconds delta)
{
    switch (state_) {
    case State::launch: {
        timer_ += delta;
        if (timer_ >= duration_) {
            timer_ = 0;
            state_ = State::ready;

            time_stream::event::DroneDeployed e;
            e.x_pos_ = grid_pos_.x;
            e.y_pos_ = grid_pos_.y;
            e.parent_near_ = parent_ == &app.player_island();
            e.destination_near_ = destination_ == &app.player_island();
            e.duration_.set(duration_);
            app.time_stream().push(app.level_timer(), e);
            break;
        }
        auto amount = smoothstep(0.f, duration_, timer_);
        auto dest = calc_pos(destination_, grid_pos_);
        auto pos = interpolate(fvec(dest),
                               Vec2<Float>{(Float)anchor_.x, (Float)anchor_.y},
                               Float(amount));
        sprite_.set_position(Vec2<Fixnum>{pos.x, pos.y});

        if (parent() not_eq &app.player_island()) {
            sprite_.set_flip({true, false});
        }
        break;
    }

    case State::ready:
        update_sprite(pfrm, app);
        break;
    }
}



} // namespace skyland
