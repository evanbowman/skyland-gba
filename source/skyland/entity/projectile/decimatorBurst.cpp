#include "decimatorBurst.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"



namespace skyland {



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
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    if (source_ not_eq &app.player_island()) {
        sprite_.set_flip({true, false});
    }

    timer_ += delta;

    if (timer_ > milliseconds(1500)) {
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

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }


    kill();
    app.camera().shake(26);
    big_explosion(pfrm, app, sprite_.get_position());

    room.apply_damage(pfrm, app, 200);
}



void DecimatorBurst::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    // Blows through drones, does not stop.

    app.camera().shake(4);

    entity.apply_damage(40);
}



} // namespace skyland
