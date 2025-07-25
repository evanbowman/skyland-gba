////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "projectile.hpp"


#include "incineratorBolt.hpp"
#include "number/fixnum.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(incinerator_impact_damage);
SHARED_VARIABLE(incinerator_splash_damage);

extern Sound cannon_sound;



IncineratorBolt::IncineratorBolt(const Vec2<Fixnum>& position,
                                 const Vec2<Fixnum>& target,
                                 Island* source,
                                 const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(78 * 2);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00030f;

    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



void IncineratorBolt::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;


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



void IncineratorBolt::rewind(Time delta)
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



void IncineratorBolt::on_collision(Room& room, Vec2<u8> origin)
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

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < 8) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    auto coord = room.position();

    room.apply_damage(incinerator_impact_damage);
    room.parent()->fire_create(coord);

    auto damage = [&](int xo, int yo) {
        Vec2<u8> c{u8(coord.x + xo), u8(coord.y + yo)};
        if (auto r = room.parent()->get_room(c)) {
            r->burn_damage(incinerator_splash_damage);
            room.parent()->fire_create(c);
        }
    };

    damage(-2, 0);
    damage(-2, 0);
    damage(-1, 0);
    damage(1, 0);
    damage(2, 0);
    damage(3, 0);
    damage(0, -1);
    damage(0, -2);
    damage(0, -3);
    damage(0, 1);
    damage(0, 2);
    damage(0, 3);
    damage(-1, -1);
    damage(-1, 1);
    damage(1, -1);
    damage(1, 1);

    make_flak_smoke(sprite_.get_position());
    make_flak_smoke(sprite_.get_position());


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



void IncineratorBolt::record_destroyed()
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
        time_stream::event::PlayerIncineratorboltDestroyed c;
        timestream_record(c);
        APP.push_time_stream(c);
    } else {
        time_stream::event::OpponentIncineratorboltDestroyed c;
        timestream_record(c);
        APP.push_time_stream(c);
    }
}



void IncineratorBolt::destroy(bool explosion)
{
    record_destroyed();

    kill();
    APP.camera()->shake(14);

    if (explosion) {
        big_explosion(sprite_.get_position());
        ExploSpawner::create(sprite_.get_position());
        if (auto sp = ExploSpawner::create(sprite_.get_position())) {
            sp->set_offset(-milliseconds(100));
        }
    }
}



void IncineratorBolt::on_collision(Entity& entity)
{
    bool skip_destroy = false;

    if (auto drone = entity.cast_drone()) {
        if (drone->ignores_damage()) {
            skip_destroy = true;
        }
        if (drone->position() == origin_tile_ and drone->parent() == source_) {
            // Do not shoot ourself.
            return;
        }
    }


    if (not skip_destroy) {
        this->destroy(true);
    }

    entity.apply_damage(3);
}



} // namespace skyland
