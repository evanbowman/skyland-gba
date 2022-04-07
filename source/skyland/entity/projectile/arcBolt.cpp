////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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



ArcBolt::ArcBolt(const Vec2<Float>& position,
                 const Vec2<Float>& target,
                 Island* source,
                 const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(78);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00011f;
    step_vector_ = direction(position, target) * speed;
}



void ArcBolt::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - app.float_delta() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    anim_timer_ -= delta;
    if (anim_timer_ < 0) {
        anim_timer_ = milliseconds(90);
        const auto kf = sprite_.get_texture_index();
        if (kf == 78) {
            sprite_.set_texture_index(79);
        } else {
            sprite_.set_texture_index(78);
        }
    }


    if (timer_ < seconds(0)) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        }
        kill();
    }
}



void ArcBolt::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + app.float_delta() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    anim_timer_ += delta;
    if (anim_timer_ > milliseconds(90)) {
        anim_timer_ = 0;
        const auto kf = sprite_.get_texture_index();
        if (kf == 78) {
            sprite_.set_texture_index(79);
        } else {
            sprite_.set_texture_index(78);
        }
    }

    Island* target;
    if (source_ == &app.player_island()) {
        target = app.opponent_island();
    } else {
        target = &app.player_island();
    }

    if (target) {
        auto t_y = (int)target->origin().y;
        auto max_y = t_y + 16 * 16 + 32;
        auto min_y = t_y + construction_zone_min_y * 16;
        int max_x = 9999999;
        int min_x = -9999999;
        if (target == &app.player_island()) {
            // If we're shooting at the player's island, the projectile moves
            // leftwards, and we care about the min bound.
            min_x = (int)target->origin().x - 32;
        } else {
            // Otherwise, we need to check the max bound.
            max_x =
                (int)target->origin().x + 16 * target->terrain().size() + 32;
        }
        if (pos.y > max_y or pos.y < min_y or pos.x > max_x or pos.x < min_x) {
            this->destroy(pfrm, app, pos.y > min_y);
            pfrm.speaker().play_sound("explosion1", 2);
        }
    }

    if (timer_ > seconds(2)) {
        kill();
    }
}



extern Sound sound_impact;



u32 flood_fill(Platform& pfrm, u8 matrix[16][16], u8 replace, u8 x, u8 y);



void ArcBolt::on_collision(Platform& pfrm, App& app, Room& room)
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
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    struct Temp
    {
        u8 matrix_[16][16];
        Buffer<Room*, 70> rooms_;
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

    flood_fill(pfrm, state->matrix_, 16, room.position().x, room.position().y);

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
        room->apply_damage(pfrm, app, arcbolt_damage);
    }


    if (str_eq(room.name(), "mirror-hull")) {
        step_vector_.x *= -1;
        step_vector_.y *= -1;
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        pfrm.speaker().play_sound("cling", 2);
    } else {
        destroy(pfrm, app, true);
        if (room.health()) {
            sound_impact.play(pfrm, 1);
        }
    }
}



void ArcBolt::destroy(Platform& pfrm, App& app, bool explosion)
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& e) {
            e.x_origin_ = origin_tile_.x;
            e.y_origin_ = origin_tile_.y;
            e.timer_.set(timer_);
            e.x_pos_.set(sprite_.get_position().x);
            e.y_pos_.set(sprite_.get_position().y);
            memcpy(&e.x_speed_, &step_vector_.x, sizeof(Float));
            memcpy(&e.y_speed_, &step_vector_.y, sizeof(Float));
        };


    if (source_ == &app.player_island()) {
        time_stream::event::PlayerArcboltDestroyed e;
        timestream_record(e);
        app.time_stream().push(app.level_timer(), e);
    } else {
        time_stream::event::OpponentArcboltDestroyed e;
        timestream_record(e);
        app.time_stream().push(app.level_timer(), e);
    }


    kill();
    app.camera()->shake(8);

    if (explosion) {
        medium_explosion(pfrm, app, sprite_.get_position());
    }
}



} // namespace skyland
