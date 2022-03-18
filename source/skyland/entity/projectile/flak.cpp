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



Flak::Flak(const Vec2<Float>& position,
           const Vec2<Float>& target,
           Island* source,
           const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(60);

    sprite_.set_origin({9, 9});

    static const Float speed = 0.00015f;
    const auto dir = direction(position, target);
    step_vector_ = dir * speed;
}



void Flak::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + app.float_delta() * step_vector_;
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
}



void Flak::destroy(Platform& pfrm, App& app, bool explosion)
{
    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x);
            c.y_pos_.set(sprite_.get_position().y);
            memcpy(&c.x_speed_, &step_vector_.x, sizeof(Float));
            memcpy(&c.y_speed_, &step_vector_.y, sizeof(Float));
        };


    if (source_ == &app.player_island()) {
        time_stream::event::PlayerFlakDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    } else {
        time_stream::event::OpponentFlakDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    }


    if (explosion) {
        explode(pfrm, app);
    }

    app.camera()->shake(8);

    kill();
}



void Flak::explode(Platform& pfrm, App& app)
{
    big_explosion(pfrm, app, sprite_.get_position());

    auto flak_smoke = [](Platform& pfrm, App& app, const Vec2<Float>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            pfrm, rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(pfrm, app, sprite_.get_position());
    flak_smoke(pfrm, app, sprite_.get_position());


    app.on_timeout(pfrm,
                   milliseconds(190),
                   [pos = sprite_.get_position(), flak_smoke](
                       Platform& pf, App& app) { flak_smoke(pf, app, pos); });
}



void Flak::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        } else if (auto drone = source_->get_drone(origin_tile_)) {
            (*drone)->___rewind___ability_used(pfrm, app);
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



void Flak::burst(Platform& pfrm,
                 App& app,
                 const Vec2<Float>& position,
                 Room& origin_room)
{
    // Ok, so now we want to find the nearest tile with which we collided.
    auto origin = origin_room.origin();
    // Go from unconstrained coordinates to an index in the opponent's tile grid.
    int y_offset = (origin.y - position.y) / 16 + 1;
    int grid_y_start = origin_room.position().y + y_offset;

    int x_offset = (origin.x - position.x) / 16;

    if (origin_room.parent() == &app.player_island()) {
        ++x_offset;
    }

    int grid_x_start = origin_room.position().x + x_offset;

    // grid_x_start = clamp(x_offset, 0, 15);
    // grid_y_start = clamp(y_offset, 0, 15);

    // Alright, now we have grid coordinates at which to apply the flak damage.

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = origin_room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto room = island->get_room({u8(x), u8(y)})) {
                room->apply_damage(pfrm, app, damage);
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



void Flak::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (destroyed_) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < flak_r1_damage) {
        room.apply_damage(pfrm, app, 9999);
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
            room.position().x + (room.size().x - 1) == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }

    Flak::burst(pfrm, app, sprite_.get_position(), room);

    if (str_eq(room.name(), "mirror-hull")) {
        explode(pfrm, app);
        app.camera()->shake(8);
        step_vector_.x *= -1;
        step_vector_.y *= -1;
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        pfrm.speaker().play_sound("cling", 2);
    } else {
        destroyed_ = true;
        destroy(pfrm, app, true);
    }
}



} // namespace skyland
