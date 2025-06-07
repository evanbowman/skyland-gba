////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "ionBurst.hpp"

#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(ion_burst_damage);



IonBurst::IonBurst(const Vec2<Fixnum>& position,
                   const Vec2<Fixnum>& target,
                   Island* source,
                   const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_texture_index(22 * 2);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



Sound sound_fizzle("fizzle");



void IonBurst::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;


    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(90)) {
        anim_timer_ = 0;
        const auto kf = sprite_.get_texture_index();
        if (kf < (22 * 2) + 2) {
            sprite_.set_texture_index(kf + 1);
        } else {
            sprite_.set_texture_index(22 * 2);
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


    if (timer_ > seconds(3)) {
        kill();
    }
}



void IonBurst::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;


    anim_timer_ -= delta;
    if (anim_timer_ < 0) {
        anim_timer_ = milliseconds(90);
        const auto kf = sprite_.get_texture_index();
        if (kf < (22 * 2) + 2) {
            sprite_.set_texture_index(kf + 1);
        } else {
            sprite_.set_texture_index(22 * 2);
        }
    }


    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used();
        }
        kill();
    }
}



void IonBurst::destroy(bool explosion)
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
        time_stream::event::PlayerIonBurstDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentIonBurstDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    }

    kill();
    APP.camera()->shake(8);
    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void IonBurst::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and room.metaclass() == ion_cannon_mt) {
        return;
    }

    if (not((*room.metaclass())->properties() &
            RoomProperties::accepts_ion_damage)) {
        return;
    }

    destroy(true);

    if ((*room.metaclass())->properties() &
        RoomProperties::cancels_ion_damage) {
        sound_fizzle.play(1);
    } else {
        room.apply_damage(ion_burst_damage);

        // A hack for the "meltdown" achievement.
        if (str_eq(room.name(), "reactor") and room.health() <= 0) {
            if (room.parent() == APP.opponent_island()) {
                achievements::raise(achievements::Achievement::meltdown);
            }
        }
    }
}



} // namespace skyland
