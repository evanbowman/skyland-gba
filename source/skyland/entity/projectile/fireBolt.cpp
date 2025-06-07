////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "fireBolt.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(firebolt_damage);
extern Sound cannon_sound;



static const Float speed = 0.00015f;



FireBolt::FireBolt(const Vec2<Fixnum>& position,
                   const Vec2<Fixnum>& target,
                   Island* source,
                   const RoomCoord& origin_tile)
    : Projectile({{6, 6}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_origin({8, 8});

    auto dir = direction(fvec(position), fvec(target));
    auto step = dir * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};

    if (dir.x > 0) {
        sprite_.set_texture_index(86);
        sprite_.set_rotation(1500 * -dir.y);
    } else {
        sprite_.set_texture_index(87);
        sprite_.set_rotation(1500 * dir.y);
    }
}



void FireBolt::set_direction(u16 rot)
{
    Vec2<Float> dir{0, -1};
    dir = rotate(dir, rot);

    auto step = dir * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};

    if (dir.x > 0) {
        sprite_.set_texture_index(86);
        sprite_.set_rotation(1500 * -dir.y);
    } else {
        sprite_.set_texture_index(87);
        sprite_.set_rotation(1500 * dir.y);
    }
}



void FireBolt::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (step_vector_.x < 0.0_fixed) {
        sprite_.set_texture_index(87);
    } else {
        sprite_.set_texture_index(86);
    }


    Island* target;
    if (is_player_island(source_)) {
        target = APP.opponent_island();
    } else {
        target = &APP.player_island();
    }

    if (target) {
        destroy_out_of_bounds(target);
    }

    if (timer_ > seconds(2)) {
        kill();
    }
}



void FireBolt::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used();
        } else if (auto drone = source_->get_drone(origin_tile_)) {
            (*drone)->___rewind___ability_used();
        }
        kill();
    }
}



extern Sound sound_impact;



void FireBolt::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent()) {
        if (room.position().x + (room.size().x - 1) == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
        if (auto origin = source_->get_room(origin_tile_)) {
            if (origin == &room) {
                return;
            }
        }
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    room.parent()->fire_create(room.position());

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < 30) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    room.apply_damage(firebolt_damage);

    if (str_eq(room.name(), "mirror-hull")) {
        room.set_ai_aware(true);
        record_destroyed();
        step_vector_.x *= Fixnum::from_integer(-1);
        step_vector_.y *= Fixnum::from_integer(-1);
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        PLATFORM.speaker().play_sound("cling", 2);
    } else {
        this->destroy(true);
        if (room.health()) {
            sound_impact.play(1);
        }
    }
}



void FireBolt::record_destroyed()
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x.as_integer());
            c.y_pos_.set(sprite_.get_position().y.as_integer());
            c.x_speed__data_.set(step_vector_.x.data());
            c.y_speed__data_.set(step_vector_.y.data());
        };


    if (is_player_island(source_)) {
        time_stream::event::PlayerFireboltDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentFireboltDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    }
}



void FireBolt::destroy(bool explosion)
{
    record_destroyed();

    kill();
    APP.camera()->shake(8);

    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void FireBolt::on_collision(Entity& entity)
{
    this->destroy(true);

    entity.apply_damage(firebolt_damage);
}



} // namespace skyland
