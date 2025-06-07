////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "drone.hpp"
#include "droneMeta.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"
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

    if (auto target = get_target()) {
        if (destination() == APP.opponent_island() and target_near_) {
            sprite_.set_flip({true, false});
        } else if (is_player_island(destination())) {
            sprite_.set_flip({false, false});
        } else {
            if (target->x < grid_pos_.x) {
                sprite_.set_flip({true, false});
            } else {
                sprite_.set_flip({false, false});
            }
        }
    }
}



Room* Drone::attached_to()
{
    for (auto& room : parent()->rooms()) {
        auto found = room->drone();
        if (found and (*found).get() == this) {
            return room.get();
        }
    }
    return nullptr;
}



void Drone::rewind(Time delta)
{
    suppress_time_stream_ = false;

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



void Drone::set_target(const TargetQueue& queue,
                       bool target_pinned,
                       bool target_near)
{
    if (queue.size() == 1) {
        set_target(queue[0].coord(), target_pinned, target_near);
        return;
    }

    clear_target_queue();

    target_pinned_ = target_pinned;

    for (int i = queue.size() - 1; i > -1; --i) {
        target_queue_.push_back(queue[i]);
    }

    target_near_ = target_near;
}



void Drone::set_target(const RoomCoord& target,
                       bool target_pinned,
                       bool target_near)
{
    auto prev_target = get_target();

    if (prev_target) {
        if (*prev_target == target and target_near_ == target_near) {
            // Optimization to save space in the rewind buffer.
            return;
        }
    }

    clear_target_queue();

    target_pinned_ = target_pinned;
    target_queue_.push_back(PackedTarget::pack(target));
    target_near_ = target_near;
}



void Drone::clear_target_queue()
{
    if (target_queue_.size() < 2) {
        time_stream::event::DroneSetTarget e;
        e.x_pos_ = grid_pos_.x;
        e.y_pos_ = grid_pos_.y;
        if (auto target = get_target()) {
            e.has_previous_target_ = true;
            e.previous_target_x_ = target->x;
            e.previous_target_y_ = target->y;
        } else {
            e.has_previous_target_ = false;
        }
        e.previous_target_near_ = target_near_;
        e.destination_near_ = is_player_island(destination_);
        APP.time_stream().push(APP.level_timer(), e);

        target_queue_.clear();

    } else {
        for (int i = target_queue_.size() - 1; i > -1; --i) {
            time_stream::event::DroneTargetQueuePop e;
            e.x_pos_ = grid_pos_.x;
            e.y_pos_ = grid_pos_.y;
            auto elem = target_queue_[i];
            e.queue_elem_x_ = elem.x_;
            e.queue_elem_y_ = elem.y_;
            e.destination_near_ = is_player_island(destination_);
            e.previous_target_near_ = target_near_;
            APP.time_stream().push(APP.level_timer(), e);
        }

        time_stream::event::DroneTargetQueueClear e;
        e.x_pos_ = grid_pos_.x;
        e.y_pos_ = grid_pos_.y;
        e.destination_near_ = is_player_island(destination_);
        APP.time_stream().push(APP.level_timer(), e);

        target_queue_.clear();
    }
}



void Drone::__rewind_push_target_queue(const RoomCoord& target)
{
    target_queue_.push_back(PackedTarget::pack(target));
}



void Drone::drop_target()
{
    if (APP.time_stream().pushes_enabled()) {
        Platform::fatal("drop_target intended to be called only during rewind, "
                        "as we do not push a time stream event in this message "
                        "for rolling back a drop_target().");
    }

    target_pinned_ = false;
    target_queue_.clear();
}



Optional<RoomCoord> Drone::get_target() const
{
    if (not target_queue_.empty()) {
        return target_queue_.back().coord();
    }
    return nullopt();
}



void Drone::set_shielded(bool v)
{
    shielded_ = v;
}



bool Drone::target_near() const
{
    return target_near_;
}



bool Drone::target_pinned() const
{
    return target_pinned_;
}



extern SharedVariable deflector_shield_strength;



void Drone::apply_damage(Health amount)
{
    if (shielded_) {
        if (amount <= deflector_shield_strength) {
            return;
        }
        amount -= deflector_shield_strength;
    }

    time_stream::event::DroneHealthChanged e;
    e.x_pos_ = grid_pos_.x;
    e.y_pos_ = grid_pos_.y;
    e.destination_near_ = is_player_island(destination_);
    e.previous_health_.set(health());
    APP.time_stream().push(APP.level_timer(), e);

    Entity::apply_damage(amount);
}



bool Drone::ignores_damage()
{
    return false;
}



void Drone::update(Time delta)
{
    switch (state_) {
    case State::launch: {
        if (timer_ == 0 and delta > 0) {
            if (not PLATFORM.speaker().is_sound_playing("tonal_flutter")) {
                PLATFORM.speaker().play_sound("tonal_flutter", 3);
            }
        }
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

            if (not suppress_time_stream_) {
                APP.time_stream().push(APP.level_timer(), e);
            }
            break;
        }
        auto amount = smoothstep(0.f, duration_, timer_);
        auto dest = calc_pos(destination_, grid_pos_);
        auto pos = interpolate_fp(dest,
                                  Vec2<Fixnum>{Fixnum::from_integer(anchor_.x),
                                               Fixnum::from_integer(anchor_.y)},
                                  Fixnum(amount));
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



void Drone::update_targets()
{
    auto isle = target_near_ ? &APP.player_island() : APP.opponent_island();
    if (not isle) {
        return;
    }

    while (target_queue_.size() > 1 and
           not isle->get_room(target_queue_.back().coord())) {

        time_stream::event::DroneTargetQueuePop e;
        e.x_pos_ = grid_pos_.x;
        e.y_pos_ = grid_pos_.y;
        auto elem = target_queue_.back();
        e.queue_elem_x_ = elem.x_;
        e.queue_elem_y_ = elem.y_;
        e.destination_near_ = is_player_island(destination_);
        e.previous_target_near_ = target_near_;
        APP.time_stream().push(APP.level_timer(), e);

        target_queue_.pop_back();
    }
}



} // namespace skyland
