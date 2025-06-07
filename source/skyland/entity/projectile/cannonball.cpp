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


#include "cannonball.hpp"
#include "number/fixnum.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(cannonball_damage);
extern Sound cannon_sound;



Cannonball::Cannonball(const Vec2<Fixnum>& position,
                       const Vec2<Fixnum>& target,
                       Island* source,
                       const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(18 * 2);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;

    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};

    set_variant(1);
}



void Cannonball::set_variant(u8 variant)
{
    variant_ = variant;

    switch (variant_) {
    case 0:
        sprite_.set_size(Sprite::Size::w8_h8);
        sprite_.set_origin({4, 4});
        sprite_.set_tidx_8x8(89, 2);
        break;

    case 1:
        if (APP.environment().is_night()) {
            // The normal cannonball texture is hard to see at night
            sprite_.set_tidx_16x16(96, 0);
        } else {
            sprite_.set_texture_index(18 * 2);
        }
        break;

    case 2:
        sprite_.set_tidx_16x16(96, 1);
        break;
    }
}



void Cannonball::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;


    if (variant_ == 2) {
        timer2_ += delta;
        if (timer2_ > milliseconds(80)) {
            timer2_ -= milliseconds(80);

            // const auto max_y =
            //     APP.player_island().origin().y + 16.0_fixed * 16.0_fixed + 32.0_fixed;

            auto pos = sprite_.get_position();
            pos = rng::sample<2>(pos, rng::utility_state);

            if (auto e = alloc_entity<Explosion>(pos)) {
                APP.effects().push(std::move(e));
            }
        }
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



void Cannonball::rewind(Time delta)
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



Sound sound_impact("impact");



SHARED_VARIABLE(sylph_cannon_damage_percent);



void Cannonball::on_collision(Room& room, Vec2<u8> origin)
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

    auto damage = (int)cannonball_damage;

    if (variant_ == 2) {
        // Unfotunately, using Fixnum here results in an unacceptable loss of
        // accuracy, so I'm using floats.
        auto damage_fp =
            ((room.max_health() * sylph_cannon_damage_percent) * 0.01f);
        damage = damage_fp;
        damage = std::max(damage, 1);
    } else if (variant_ == 0) {
        damage /= 2;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() <= damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    room.apply_damage(damage,
                      {
                          .ignore_deflector_shield_ = variant_ == 2,
                      });

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
        bool big_explo = damage > 40;
        this->destroy(not big_explo);
        if (big_explo) {
            big_explosion(sprite_.get_position(),
                          {
                              .centerflash_ = true,
                          });
        }
        if (room.health()) {
            sound_impact.play(1);
        }
    }
}



void Cannonball::record_destroyed()
{
    auto timestream_record =
        [&](time_stream::event::BasicCannonballDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x.as_integer());
            c.y_pos_.set(sprite_.get_position().y.as_integer());
            c.x_speed__data_.set(step_vector_.x.data());
            c.y_speed__data_.set(step_vector_.y.data());
        };


    if (is_player_island(source_)) {
        time_stream::event::PlayerCannonballDestroyed c;
        timestream_record(c);
        c.variant_ = variant_;
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentCannonballDestroyed c;
        timestream_record(c);
        c.variant_ = variant_;
        APP.time_stream().push(APP.level_timer(), c);
    }
}



void Cannonball::destroy(bool explosion)
{
    record_destroyed();

    kill();
    APP.camera()->shake(8);

    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void Cannonball::on_collision(Entity& entity)
{
    // FIXME: Probably slow... but then... in most cases it only happens once,
    // as the Cannonball explodes upon collision.

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

    auto damage = (int)cannonball_damage;

    if (variant_ == 2) {

    } else if (variant_ == 0) {
        damage /= 2;
    }

    entity.apply_damage(damage);
}



} // namespace skyland
