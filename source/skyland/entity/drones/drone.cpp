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


#include "drone.hpp"
#include "droneMeta.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/room.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static Vec2<Fixnum> calc_pos(Island* island, const RoomCoord& grid_coord)
{
    auto o = island->visual_origin();
    o.x += Fixnum::from_integer(grid_coord.x * 16);
    o.y += Fixnum::from_integer(grid_coord.y * 16);
    return o;
}



Drone::Drone(const char* name,
             Island* parent,
             Island* destination,
             const RoomCoord& grid_pos)
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



void Drone::set_movement_target(const RoomCoord& position)
{
    auto old_pos = grid_pos_;
    grid_pos_ = position;

    auto v1 = calc_pos(parent_, old_pos);
    auto v2 = calc_pos(destination_, grid_pos_);

    auto dist = distance(fvec(v1), fvec(v2));

    auto speed = 0.0002f;

    duration_ = dist / speed;
}



void Drone::update_sprite()
{
    auto o = calc_pos(destination_, grid_pos_);

    Float offset = 3 * float(sine(4 * 3.14f * 0.0005f * duration_ + 180)) /
                   std::numeric_limits<s16>::max();

    if (PLATFORM.network_peer().is_connected()) {
        // The floating movement complicates proper collision checking, as
        // minute differences in the game clocks in multiplayer mode can result
        // in different collision results in each game, if the drone moves
        // subtly up and down.
        offset = 0;
    }

    o.y += Fixnum(offset);

    sprite_.set_position(o);

    if (target_) {
        if (destination() == APP.opponent_island() and target_near_) {
            sprite_.set_flip({true, false});
        } else if (is_player_island(destination())) {
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



void Drone::rewind(Microseconds delta)
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
                            Vec2<Float>{(Float)anchor_.x, (Float)anchor_.y},
                            Float(amount));

            sprite_.set_position(Vec2<Fixnum>{Fixnum(pos.x), Fixnum(pos.y)});

            if (parent() not_eq &APP.player_island()) {
                sprite_.set_flip({true, false});
            }
        }
        break;
    }

    default:
        update_sprite();
        if (timer_ > 0) {
            timer_ -= delta;
        }
        break;
    }
}



void Drone::set_target(const RoomCoord& target, bool target_near)
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
    e.destination_near_ = is_player_island(destination_);
    APP.time_stream().push(APP.level_timer(), e);

    target_ = target;
    target_near_ = target_near;
}



void Drone::drop_target()
{
    if (APP.time_stream().pushes_enabled()) {
        Platform::fatal("drop_target intended to be called only during rewind, "
                        "as we do not push a time stream event in this message "
                        "for rolling back a drop_target().");
    }

    target_.reset();
}



void Drone::apply_damage(Health amount)
{
    time_stream::event::DroneHealthChanged e;
    e.x_pos_ = grid_pos_.x;
    e.y_pos_ = grid_pos_.y;
    e.destination_near_ = is_player_island(destination_);
    e.previous_health_.set(health());
    APP.time_stream().push(APP.level_timer(), e);

    Entity::apply_damage(amount);
}



void Drone::update(Microseconds delta)
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
            e.parent_near_ = is_player_island(parent_);
            e.destination_near_ = is_player_island(destination_);
            e.duration_.set(duration_);
            APP.time_stream().push(APP.level_timer(), e);
            break;
        }
        auto amount = smoothstep(0.f, duration_, timer_);
        auto dest = calc_pos(destination_, grid_pos_);
        auto pos = interpolate(fvec(dest),
                               Vec2<Float>{(Float)anchor_.x, (Float)anchor_.y},
                               Float(amount));
        sprite_.set_position(Vec2<Fixnum>{Fixnum(pos.x), Fixnum(pos.y)});

        if (parent() not_eq &APP.player_island()) {
            sprite_.set_flip({true, false});
        }
        break;
    }

    case State::ready:
        update_sprite();
        break;
    }
}



void Drone::display(Platform::Screen& screen)
{
    screen.draw(sprite());

    if (state_ == State::launch) {
        const auto cached_pos = sprite_.get_position();

        auto pos = calc_pos(destination_, grid_pos_);
        sprite_.set_position(pos);
        sprite_.set_alpha(Sprite::Alpha::translucent);
        screen.draw(sprite_);

        sprite_.set_position(cached_pos);
        sprite_.set_alpha(Sprite::Alpha::opaque);
    }
}



} // namespace skyland
