////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "flak.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(flak_r1_damage);
SHARED_VARIABLE(flak_r2_damage);
SHARED_VARIABLE(flak_r3_damage);



static Sound sound_fizzle("fizzle");



Flak::Flak(const Vec2<Fixnum>& position,
           const Vec2<Fixnum>& target,
           Island* source,
           const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(18 * 2 + 1);

    sprite_.set_origin({9, 9});

    static const Float speed = 0.00015f;
    const auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



void Flak::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (timer_ > seconds(2)) {
        kill();
    }

    flicker_time_ += delta;
    if (flicker_time_ > milliseconds(150)) {
        flicker_time_ = 0;

        if (sprite_.get_mix().amount_) {
            sprite_.set_mix({});
        } else {
            sprite_.set_mix({ColorConstant::silver_white, 200});
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
}



void Flak::record_destroyed()
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
        time_stream::event::PlayerFlakDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentFlakDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    }
}



void Flak::destroy(bool explosion)
{
    record_destroyed();

    if (explosion) {
        explode();
    }

    APP.camera()->shake(8);

    kill();
}



void make_flak_smoke(const Vec2<Fixnum>& pos)
{
    if (SmokePuff::get_instance_count() >= 9) {
        // For better framerate, don't create too many of these.
        return;
    }

    auto e = APP.alloc_entity<SmokePuff>(
        rng::sample<48>(pos, rng::utility_state), 61);

    if (e) {
        APP.effects().push(std::move(e));
    }
}



void Flak::explode()
{
    big_explosion(sprite_.get_position());

    make_flak_smoke(sprite_.get_position());


    Vec2<s32> pos;
    pos.x = sprite_.get_position().x.as_integer();
    pos.y = sprite_.get_position().y.as_integer();

    APP.on_timeout(milliseconds(190), [pos]() {
        Vec2<Fixnum> p;
        p.x = Fixnum::from_integer(pos.x);
        p.y = Fixnum::from_integer(pos.y);
        make_flak_smoke(p);
    });
}



void Flak::rewind(Time delta)
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

    flicker_time_ -= delta;
    if (flicker_time_ < 0) {
        flicker_time_ = milliseconds(150);

        if (sprite_.get_mix().amount_) {
            sprite_.set_mix({});
        } else {
            sprite_.set_mix({ColorConstant::silver_white, 200});
        }
    }
}



void Flak::burst(const Vec2<Fixnum>& position,
                 Room& origin_room,
                 Island* source)
{

    int grid_x_start = origin_room.position().x;
    int grid_y_start = origin_room.position().y;

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = origin_room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto room = island->get_room({u8(x), u8(y)})) {
                room->apply_damage(damage);
            }
        }
    };

    // Apply damage in this pattern:
    //         *
    //       * * *
    //     * * * * *
    //       * * *
    //         *
    //
    // More damage at center of explosion.

    apply_damage(0, 0, flak_r1_damage);

    apply_damage(1, 0, flak_r2_damage);
    apply_damage(-1, 0, flak_r2_damage);
    apply_damage(0, 1, flak_r2_damage);
    apply_damage(0, -1, flak_r2_damage);

    apply_damage(2, 0, flak_r3_damage);
    apply_damage(1, -1, flak_r3_damage);
    apply_damage(0, -2, flak_r3_damage);
    apply_damage(-1, -1, flak_r3_damage);
    apply_damage(-2, 0, flak_r3_damage);
    apply_damage(-1, 1, flak_r3_damage);
    apply_damage(0, 2, flak_r3_damage);
    apply_damage(1, 1, flak_r3_damage);
}



void Flak::on_collision(Room& room, Vec2<u8> origin)
{
    if (destroyed_) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < flak_r1_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    if (source_ == room.parent()) {
        if (auto origin = source_->get_room(origin_tile_)) {
            if (origin == &room) {
                return;
            }
        }
        if (room.position().x == origin_tile_.x or
            room.position().x + 1 == origin_tile_.x or
            room.position().x + (room.size().x - 1) == origin_tile_.x or
            room.position().x == origin_tile_.x + 1) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    Flak::burst(sprite_.get_position(), room, source_);

    if (str_eq(room.name(), "mirror-hull")) {
        room.set_ai_aware(true);
        record_destroyed();
        explode();
        APP.camera()->shake(8);
        step_vector_.x *= Fixnum::from_integer(-1);
        step_vector_.y *= Fixnum::from_integer(-1);
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        PLATFORM.speaker().play_sound("cling", 2);
    } else {
        destroyed_ = true;
        destroy(true);
    }
}



} // namespace skyland
