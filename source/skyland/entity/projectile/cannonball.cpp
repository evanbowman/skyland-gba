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


#include "projectile.hpp"


#include "cannonball.hpp"
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
#include "number/fixed.hpp"



namespace skyland
{



SHARED_VARIABLE(cannonball_damage);
extern Sound cannon_sound;



Cannonball::Cannonball(const Vec2<Fixnum>& position,
                       const Vec2<Fixnum>& target,
                       Island* source,
                       const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(18);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;

    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{step.x, step.y};
}



void Cannonball::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Fixnum(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    // {
    //     auto pos = sprite_.get_position();
    //     Vec2<Fixnum> testp {pos.x, pos.y};

    //     Vec2<Fixnum> tests { step_vector_.x, step_vector_.y };

    //     const auto begin = pfrm.delta_clock().sample();

    //     testp = testp + Fixnum(delta) * tests;

    //     const auto end = pfrm.delta_clock().sample();
    //     info(pfrm, stringify(end - begin).c_str());
    //     info(pfrm, stringify(testp.x.as_integer()).c_str());
    // }


    // Island* target;
    // if (source_ == &app.player_island()) {
    //     target = app.opponent_island();
    // } else {
    //     target = &app.player_island();
    // }

    // if (target) {
    //     auto t_y = target->origin().y.as_integer();
    //     auto max_y = t_y + 16 * 16 + 32;
    //     auto min_y = t_y + construction_zone_min_y * 16;
    //     int max_x = 9999999;
    //     int min_x = -9999999;
    //     if (target == &app.player_island()) {
    //         // If we're shooting at the player's island, the projectile moves
    //         // leftwards, and we care about the min bound.
    //         min_x = target->origin().x.as_integer() - 32;
    //     } else {
    //         // Otherwise, we need to check the max bound.
    //         max_x =
    //             target->origin().x.as_integer() + 16 * target->terrain().size() + 32;
    //     }
    //     if (pos.y > max_y or pos.y < min_y or pos.x > max_x or pos.x < min_x) {
    //         this->destroy(pfrm, app, pos.y > min_y);
    //         pfrm.speaker().play_sound("explosion1", 2);
    //     }
    // }

    if (timer_ > seconds(2)) {
        kill();
    }
}



void Cannonball::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    // auto pos = sprite_.get_position();
    // pos = pos - app.float_delta() * step_vector_;
    // sprite_.set_position(pos);

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



Sound sound_impact("impact");



void Cannonball::on_collision(Platform& pfrm, App& app, Room& room)
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
        room.max_health() < cannonball_damage) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    room.apply_damage(pfrm, app, cannonball_damage);

    if (str_eq(room.name(), "mirror-hull")) {
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



void Cannonball::destroy(Platform& pfrm, App& app, bool explosion)
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x.as_integer());
            c.y_pos_.set(sprite_.get_position().y.as_integer());
            auto sx = step_vector_.x.as_float();
            auto sy = step_vector_.y.as_float();
            memcpy(&c.x_speed_, &sx, sizeof(Float));
            memcpy(&c.y_speed_, &sy, sizeof(Float));
        };


    if (source_ == &app.player_island()) {
        time_stream::event::PlayerCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    } else {
        time_stream::event::OpponentCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    }


    kill();
    app.camera()->shake(8);

    if (explosion) {
        medium_explosion(pfrm, app, sprite_.get_position());
    }
}



void Cannonball::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    // FIXME: Probably slow... but then... in most cases it only happens once,
    // as the Cannonball explodes upon collision.
    if (auto drone = dynamic_cast<Drone*>(&entity)) {
        if (drone->position() == origin_tile_ and drone->parent() == source_) {
            // Do not shoot ourself.
            return;
        }
    }


    this->destroy(pfrm, app, true);

    entity.apply_damage(pfrm, app, cannonball_damage);
}



} // namespace skyland
