////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "curveshot.hpp"


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



const int curveshot_damage = 50;



Curveshot::Curveshot(const Vec2<Float>& position,
                     const Vec2<Float>& target,
                     Island* source,
                     Island* dest,
                     const RoomCoord& origin_tile,
                     const RoomCoord& dest_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(18);
    y_base_ = position.y;

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    step_vector_ = direction(position, target) * speed;

    // 6,850,350
    time_to_target_ = distance(position, target) / speed;
    height_ = (14 - dest_tile.y) * 16;
}



void Curveshot::update(Time delta)
{
    Vec2<Float> pos = {sprite_.get_position().x, y_base_};
    pos = pos + APP.float_delta() * step_vector_;
    y_base_ = pos.y;

    pos.y -= height_ *
             Float(sine((Float(timer_) / time_to_target_) *
                        (std::numeric_limits<s16>::max() / 2))) /
             std::numeric_limits<s16>::max();

    sprite_.set_position(pos);

    timer_ += delta;


    Island* target;
    if (is_player_island(source_)) {
        target = APP.opponent_island();
    } else {
        target = &APP.player_island();
    }

    if (target) {
        auto t_y = (int)target->origin().y;
        auto max_y = t_y + 16 * 16 + 32;
        auto min_y = t_y + construction_zone_min_y * 16;
        int max_x = 9999999;
        int min_x = -9999999;
        if (target == &APP.player_island()) {
            // If we're shooting at the player's island, the projectile moves
            // leftwards, and we care about the min bound.
            min_x = (int)target->origin().x - 32;
        } else {
            // Otherwise, we need to check the max bound.
            max_x =
                (int)target->origin().x + 16 * target->terrain().size() + 32;
        }
        if (pos.y > max_y or pos.y < min_y or pos.x > max_x or pos.x < min_x) {
            this->destroy(pos.y > min_y);
            PLATFORM.speaker().play_sound("explosion1", 2);
        }
    }

    if (timer_ > seconds(2)) {
        kill();
    }
}



void Curveshot::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - Float(delta) * step_vector_;
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



void Curveshot::on_collision(Room& room)
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
        room.max_health() < curveshot_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    room.apply_damage(curveshot_damage);

    if (str_eq(room.name(), "mirror-hull")) {
        room.set_ai_aware(true);
        step_vector_.x *= -1;
        step_vector_.y *= -1;
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



void Curveshot::destroy(bool explosion)
{
    // auto timestream_record =
    //     [&](time_stream::event::BasicProjectileDestroyed& c) {
    //         c.x_origin_ = origin_tile_.x;
    //         c.y_origin_ = origin_tile_.y;
    //         c.timer_.set(timer_);
    //         c.x_pos_.set(sprite_.get_position().x);
    //         c.y_pos_.set(sprite_.get_position().y);
    //         memcpy(&c.x_speed_, &step_vector_.x, sizeof(Float));
    //         memcpy(&c.y_speed_, &step_vector_.y, sizeof(Float));
    //     };


    // if (source_ == &APP.player_island()) {
    //     time_stream::event::PlayerCurveshotDestroyed c;
    //     timestream_record(c);
    //     APP.time_stream().push(APP.level_timer(), c);
    // } else {
    //     time_stream::event::OpponentCurveshotDestroyed c;
    //     timestream_record(c);
    //     APP.time_stream().push(APP.level_timer(), c);
    // }


    kill();
    APP.camera()->shake(8);

    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void Curveshot::on_collision(Entity& entity)
{
    // FIXME: Probably slow... but then... in most cases it only happens once,
    // as the Curveshot explodes upon collision.

    this->destroy(true);

    entity.apply_damage(curveshot_damage);
}



} // namespace skyland
