#include "decimatorBurst.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static SHARED_VARIABLE(decimator_burst_damage);



DecimatorBurst::DecimatorBurst(const Vec2<Float>& position,
                               const Vec2<Float>& target,
                               Island* source,
                               const Vec2<u8>& origin_tile)
    : Projectile({{10, 20}, {8, 16}}), source_(source),
      origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(72);

    sprite_.set_origin({8, 16});

    static const Float speed = 0.00025f;
    step_vector_ = direction(position, target) * speed;
}



void DecimatorBurst::update(Platform&, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + app.float_delta() * step_vector_;
    sprite_.set_position(pos);

    if (source_ not_eq &app.player_island()) {
        sprite_.set_flip({true, false});
    }

    timer_ += delta;

    Island* target;
    if (source_ == &app.player_island()) {
        target = app.opponent_island();
    } else {
        target = &app.player_island();
    }

    if (target) {
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
        if (pos.x > max_x or pos.x < min_x) {
            kill();
        }
    }


    if (timer_ > milliseconds(1500)) {
        kill();
    }
}



void DecimatorBurst::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (source_ not_eq &app.player_island()) {
        sprite_.set_flip({true, false});
    }

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        }
        kill();
    }
}



void DecimatorBurst::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent()) {
        if (room.position().x == origin_tile_.x or
            room.position().x + 1 == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < decimator_burst_damage) {
        room.apply_damage(pfrm, app, 9999);
        return;
    }

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }


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
        time_stream::event::PlayerDecimatorBurstDestroyed e;
        timestream_record(e);
        app.time_stream().push(pfrm, app.level_timer(), e);
    } else {
        time_stream::event::OpponentDecimatorBurstDestroyed e;
        timestream_record(e);
        app.time_stream().push(pfrm, app.level_timer(), e);
    }


    kill();
    app.camera()->shake(26);
    big_explosion(pfrm, app, sprite_.get_position());

    room.apply_damage(pfrm, app, decimator_burst_damage);
}



void DecimatorBurst::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    // Blows through drones, does not stop.

    app.camera()->shake(4);

    entity.apply_damage(pfrm, app, decimator_burst_damage);
}



} // namespace skyland
