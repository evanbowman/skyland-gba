#include "fireBolt.hpp"
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



extern Sound cannon_sound;



FireBolt::FireBolt(const Vec2<Float>& position,
                       const Vec2<Float>& target,
                       Island* source,
                       const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    auto dir = direction(position, target);
    step_vector_ = dir * speed;

    if (dir.x > 0) {
        sprite_.set_texture_index(86);
        sprite_.set_rotation(1500 * -dir.y);
    } else {
        sprite_.set_texture_index(87);
        sprite_.set_rotation(1500 * dir.y);
    }

}



void FireBolt::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (step_vector_.x < 0) {
        sprite_.set_texture_index(87);
    } else {
        sprite_.set_texture_index(86);
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



void FireBolt::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - app.float_delta() * step_vector_;
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
}



extern Sound sound_impact;



void FireBolt::on_collision(Platform& pfrm, App& app, Room& room)
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

    room.parent()->fire_create(pfrm, app, room.position());

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < 30) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    room.apply_damage(pfrm, app, 30);

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



void FireBolt::destroy(Platform& pfrm, App& app, bool explosion)
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
        time_stream::event::PlayerFireboltDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    } else {
        time_stream::event::OpponentFireboltDestroyed c;
        timestream_record(c);
        app.time_stream().push(app.level_timer(), c);
    }


    kill();
    app.camera()->shake(8);

    if (explosion) {
        medium_explosion(pfrm, app, sprite_.get_position());
    }
}



void FireBolt::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    if (auto drone = dynamic_cast<Drone*>(&entity)) {
        if (drone->position() == origin_tile_ and drone->parent() == source_) {
            // Do not shoot ourself.
            return;
        }
    }


    this->destroy(pfrm, app, true);

    entity.apply_damage(pfrm, app, 30);
}



} // namespace skyland