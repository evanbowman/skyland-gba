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


#include "beam.hpp"
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



SHARED_VARIABLE(beam_damage);



Beam::Beam(const Vec2<Fixnum>& position,
           const Vec2<Fixnum>& target,
           Island* source,
           const RoomCoord& origin_tile,
           int index)
    : Projectile({{10, 10}, {8, 8}}), source_(source),
      origin_tile_(origin_tile), index_(index)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(60 * 2);
    if (index == 1) {
        sprite_.set_texture_index(79 * 2);
    }
    if (index > 4) {
        sprite_.set_texture_index(60 * 2 + 1);
    }
    if (index == 9) {
        sprite_.set_texture_index(79 * 2 + 1);
    }

    sprite_.set_origin({8, 8});

    static const Float speed = 0.0002f;

    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};

    if (source == &player_island()) {
        sprite_.set_palette(2);
    }
}



void Beam::update(Time delta)
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



void Beam::rewind(Time delta)
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



void Beam::on_collision(Room& room, Vec2<u8> origin)
{
    if (index_ > 1) {
        return;
    }

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

    if (source_ not_eq room.parent()) {
        if (not hit_opponent_) {
            // Both islands use the same coordinate system. We wouldn't want to
            // ignore a coordinate because we already hit it on the player's
            // island.
            hit_opponent_ = true;
            damaged_.clear();
        }
    }

    bool damaged = false;
    for (auto& c : damaged_) {
        if (c == room.position()) {
            damaged = true;
        }
    }

    if (not damaged) {
        sound_impact.play(1);
        if (APP.camera()->shake_magnitude() <= 2) {
            APP.camera()->shake(8);
        }
        room.apply_damage(beam_damage);
        if (not damaged_.push_back(room.position())) {
            this->destroy(true);
        }
    }
}



void Beam::restore_blocks_hit(const time_stream::event::BeamDestroyed& e)
{
    for (u32 i = 0; i < e.hit_count_; ++i) {
        damaged_.push_back({e.blocks_hit_[i].x_, e.blocks_hit_[i].y_});
    }
}



void Beam::record_destroyed()
{
    auto timestream_record = [&](time_stream::event::BeamDestroyed& c) {
        c.x_origin_ = origin_tile_.x;
        c.y_origin_ = origin_tile_.y;
        c.timer_.set(timer_);
        c.x_pos_.set(sprite_.get_position().x.as_integer());
        c.y_pos_.set(sprite_.get_position().y.as_integer());
        c.x_speed__data_.set(step_vector_.x.data());
        c.y_speed__data_.set(step_vector_.y.data());
        c.index_ = index_;

        c.hit_count_ = damaged_.size();
        for (u32 i = 0; i < damaged_.size(); ++i) {
            c.blocks_hit_[i].x_ = damaged_[i].x;
            c.blocks_hit_[i].y_ = damaged_[i].y;
        }
    };


    if (is_player_island(source_)) {
        time_stream::event::PlayerBeamDestroyed c;
        timestream_record(c);
        APP.push_time_stream(c);
    } else {
        time_stream::event::OpponentBeamDestroyed c;
        timestream_record(c);
        APP.push_time_stream(c);
    }
}



void Beam::destroy(bool explosion)
{
    record_destroyed();

    kill();

    APP.camera()->shake(8);

    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void Beam::on_collision(Entity& entity)
{
    // FIXME: Probably slow... but then... in most cases it only happens once,
    // as the Beam explodes upon collision.
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

    entity.apply_damage(10);
}



} // namespace skyland
