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



extern Sound cannon_sound;



FireBolt::FireBolt(const Vec2<Fixnum>& position,
                   const Vec2<Fixnum>& target,
                   Island* source,
                   const RoomCoord& origin_tile)
    : Projectile({{6, 6}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    auto dir = direction(fvec(position), fvec(target));
    auto step = dir * speed;
    step_vector_ = Vec2<Fixnum>{step.x, step.y};

    if (dir.x > 0) {
        sprite_.set_texture_index(86);
        sprite_.set_rotation(1500 * -dir.y);
    } else {
        sprite_.set_texture_index(87);
        sprite_.set_rotation(1500 * dir.y);
    }
}



void FireBolt::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + app.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (step_vector_.x < 0) {
        sprite_.set_texture_index(87);
    } else {
        sprite_.set_texture_index(86);
    }


    Island* target;
    if (source_ == &app.player_island()) {
        target = app.opponent_island();
    } else {
        target = &app.player_island();
    }

    if (target) {
        destroy_out_of_bounds(pfrm, app, target);
    }

    if (timer_ > seconds(2)) {
        kill();
    }
}



void FireBolt::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - app.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        } else if (auto drone = source_->get_drone(origin_tile_)) {
            (*drone)->___rewind___ability_used(pfrm, app);
        }
        kill();
    }
}



extern Sound sound_impact;



void FireBolt::on_collision(Platform& pfrm, App& app, Room& room)
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

    room.parent()->fire_create(pfrm, app, room.position());

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < 30) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    room.apply_damage(pfrm, app, 30);

    if (str_eq(room.name(), "mirror-hull")) {
        record_destroyed(pfrm, app);
        step_vector_.x *= -1;
        step_vector_.y *= -1;
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        pfrm.speaker().play_sound("cling", 2);
    } else {
        this->destroy(pfrm, app, true);
        if (room.health()) {
            sound_impact.play(pfrm, 1);
        }
    }
}



void FireBolt::record_destroyed(Platform& pfrm, App& app)
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


    if (source_ == &app.player_island()) {
        time_stream::event::PlayerFireboltDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    } else {
        time_stream::event::OpponentFireboltDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    }
}



void FireBolt::destroy(Platform& pfrm, App& app, bool explosion)
{
    record_destroyed(pfrm, app);

    kill();
    app.camera()->shake(8);

    if (explosion) {
        medium_explosion(pfrm, app, sprite_.get_position());
    }
}



void FireBolt::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    this->destroy(pfrm, app, true);

    entity.apply_damage(pfrm, app, 30);
}



} // namespace skyland
