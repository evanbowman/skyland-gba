////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "arcBolt.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(arcbolt_damage);



ArcBolt::ArcBolt(const Vec2<Fixnum>& position,
                 const Vec2<Fixnum>& target,
                 Island* source,
                 const RoomCoord& origin_tile)
    : Projectile({{8, 8}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(26 * 2);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00011f;
    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



ArcBolt::ArcBolt(const Vec2<Fixnum>& position,
                 int dir,
                 Island* source,
                 const RoomCoord& origin_tile)
    : Projectile({{8, 8}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(26 * 2);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00011f;
    auto step = rotate({1, 0}, dir) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



void ArcBolt::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    anim_timer_ -= delta;
    if (anim_timer_ < 0) {
        anim_timer_ = milliseconds(90);
        const auto kf = sprite_.get_texture_index();
        if (kf == 26 * 2) {
            sprite_.set_texture_index(26 * 2 + 1);
        } else {
            sprite_.set_texture_index(26 * 2);
        }
    }


    if (timer_ < seconds(0)) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used();
        }
        kill();
    }
}



void ArcBolt::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(90)) {
        anim_timer_ = 0;
        const auto kf = sprite_.get_texture_index();
        if (kf == 26 * 2) {
            sprite_.set_texture_index(26 * 2 + 1);
        } else {
            sprite_.set_texture_index(26 * 2);
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



extern Sound sound_impact;



u32 flood_fill(u8 matrix[16][16], u8 replace, u8 x, u8 y);



void ArcBolt::on_collision(Room& room, Vec2<u8> origin)
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
        room.max_health() < arcbolt_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    struct Temp
    {
        u8 matrix_[16][16];
        Buffer<Room*, 90> rooms_;
    };

    auto state = allocate_dynamic<Temp>("arcbolt-fill-buffer");
    room.parent()->plot_rooms(state->matrix_);

    // Remove any room from plot if type differs from colliding room type.
    for (u32 x = 0; x < room.parent()->terrain().size(); ++x) {
        for (int y = 0; y < 16; ++y) {
            if (state->matrix_[x][y]) {
                if (room.parent()->get_room({u8(x), u8(y)})->metaclass() not_eq
                    room.metaclass()) {
                    state->matrix_[x][y] = 0;
                }
            }
        }
    }

    flood_fill(state->matrix_, 16, room.position().x, room.position().y);

    for (u32 x = 0; x < room.parent()->terrain().size(); ++x) {
        for (int y = 0; y < 16; ++y) {
            if (state->matrix_[x][y] == 16) {
                if (auto r = room.parent()->get_room({u8(x), u8(y)})) {
                    bool found = false;
                    for (auto& room : state->rooms_) {
                        if (room == r) {
                            found = true;
                            break;
                        }
                    }
                    if (not found) {
                        state->rooms_.push_back(r);
                    }
                }
            }
        }
    }

    for (auto& room : state->rooms_) {
        room->apply_damage(arcbolt_damage);
    }


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
        destroy(true);
        if (room.health()) {
            sound_impact.play(1);
        }
    }
}



void ArcBolt::record_destroyed()
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& e) {
            e.x_origin_ = origin_tile_.x;
            e.y_origin_ = origin_tile_.y;
            e.timer_.set(timer_);
            e.x_pos_.set(sprite_.get_position().x.as_integer());
            e.y_pos_.set(sprite_.get_position().y.as_integer());
            e.x_speed__data_.set(step_vector_.x.data());
            e.y_speed__data_.set(step_vector_.y.data());
        };


    if (is_player_island(source_)) {
        time_stream::event::PlayerArcboltDestroyed e;
        timestream_record(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentArcboltDestroyed e;
        timestream_record(e);
        APP.time_stream().push(APP.level_timer(), e);
    }
}



void ArcBolt::destroy(bool explosion)
{
    record_destroyed();

    kill();
    APP.camera()->shake(8);

    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



} // namespace skyland
