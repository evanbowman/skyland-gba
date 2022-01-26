#include "projectile.hpp"


#include "cannonball.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



SHARED_VARIABLE(cannonball_damage);



Cannonball::Cannonball(const Vec2<Float>& position,
                       const Vec2<Float>& target,
                       Island* source,
                       const Vec2<u8>& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(18);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    step_vector_ = direction(position, target) * speed;
}



void Cannonball::update(Platform&, App&, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (timer_ > seconds(2)) {
        kill();
    }
}



void Cannonball::rewind(Platform& pfrm, App& app, Microseconds delta)
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
    }

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }

    if ((*room.metaclass())->category() == Room::Category::decoration) {
        room.apply_damage(pfrm, app, 10000);
        return;
    }


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
        time_stream::event::PlayerCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    } else {
        time_stream::event::OpponentCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    }


    kill();
    app.camera().shake(8);
    medium_explosion(pfrm, app, sprite_.get_position());

    room.apply_damage(pfrm, app, cannonball_damage);

    if (room.health()) {
        sound_impact.play(pfrm, 1);
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
        time_stream::event::PlayerCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    } else {
        time_stream::event::OpponentCannonballDestroyed c;
        timestream_record(c);
        app.time_stream().push(pfrm, app.level_timer(), c);
    }



    kill();
    app.camera().shake(8);
    medium_explosion(pfrm, app, sprite_.get_position());

    entity.apply_damage(pfrm, app, cannonball_damage);
}



} // namespace skyland
