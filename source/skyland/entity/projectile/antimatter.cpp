////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "antimatter.hpp"

#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/bulkhead.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Antimatter::Antimatter(const Vec2<Fixnum>& position,
                       const Vec2<Fixnum>& target,
                       Island* source,
                       const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h16);
    sprite_.set_tidx_16x16(95, 0);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



extern Sound sound_fizzle;



void Antimatter::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;


    // anim_timer_ += delta;
    // if (anim_timer_ > milliseconds(90)) {
    //     anim_timer_ = 0;
    //     const auto kf = sprite_.get_texture_index();
    //     if (kf < 58) {
    //         sprite_.set_texture_index(kf + 1);
    //     } else {
    //         sprite_.set_texture_index(56);
    //     }
    // }

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



void Antimatter::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;


    // anim_timer_ -= delta;
    // if (anim_timer_ < 0) {
    //     anim_timer_ = milliseconds(90);
    //     const auto kf = sprite_.get_texture_index();
    //     if (kf < 58) {
    //         sprite_.set_texture_index(kf + 1);
    //     } else {
    //         sprite_.set_texture_index(56);
    //     }
    // }


    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used();
        }
        kill();
    }
}



void Antimatter::destroy(bool explosion)
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
        time_stream::event::PlayerAntimatterDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentAntimatterDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    }

    kill();
    APP.camera()->shake(8);
    if (explosion) {
        medium_explosion(sprite_.get_position());
    }
}



void Antimatter::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and str_eq(room.name(), "annihilator")) {
        return;
    }

    const bool is_energized_hull = str_eq(room.name(), "energized-hull");

    if (str_eq(room.name(), "forcefield") or
        str_eq(room.name(), "forcefield*") or is_energized_hull) {

        int range = 2;


        if (is_energized_hull) {
            room.apply_damage(70);
            range = 1;
        } else {
            room.apply_damage(9999);
        }

        make_flak_smoke(sprite_.get_position());
        make_flak_smoke(sprite_.get_position());

        big_explosion(sprite_.get_position());

        const auto pos = ivec(sprite_.get_position());

        APP.on_timeout(milliseconds(190), [pos]() {
            Vec2<Fixnum> p;
            p.x = pos.x;
            p.y = pos.y;
            make_flak_smoke(p);
        });

        auto targets =
            allocate_dynamic<Buffer<Room*, 300>>("antimatter-target-buffer");

        static const Health damage = 58;

        for (int x = -range; x < range + 1; ++x) {
            for (int y = -range; y < range + 1; ++y) {
                if (room.position().x + x < 0 or room.position().y + y < 0 or
                    room.position().x + x > 15 or room.position().y + y > 15) {
                    continue;
                }

                auto pos = RoomCoord{u8(room.position().x + x),
                                     u8(room.position().y + y)};

                if (auto other = room.parent()->get_room(pos)) {
                    bool found = false;
                    for (auto& target : *targets) {
                        if (target == other) {
                            found = true;
                            break;
                        }
                    }
                    if (not found) {
                        targets->push_back(other);
                    }
                } else if (auto drone = room.parent()->get_drone(pos)) {
                    (*drone)->apply_damage(damage);
                }
            }
        }

        for (auto& room : *targets) {
            room->apply_damage(damage);

            if (not((*room->metaclass())->properties() &
                    RoomProperties::fireproof)) {
                if (room->health() > 0) {
                    room->parent()->fire_create(room->position());
                }
            }
        }
    } else {
        room.apply_damage(10);
    }

    destroy(true);
}



} // namespace skyland
