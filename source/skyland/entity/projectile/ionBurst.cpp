#include "ionBurst.hpp"

#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/room_metatable.hpp"



namespace skyland {



IonBurst::IonBurst(const Vec2<Float>& position,
                       const Vec2<Float>& target,
                       Island* source)
    : Projectile({{10, 10}, {8, 8}}), source_(source)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(18);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00015f;
    step_vector_ = direction(position, target) * speed;
}



void IonBurst::update(Platform&, App&, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (timer_ > seconds(1)) {
        kill();
    }
}



void IonBurst::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and room.metaclass() == ion_cannon_mt) {
        return;
    }

    if (room.metaclass() not_eq forcefield_mt) {
        return;
    }


    kill();
    app.camera().shake(8);
    medium_explosion(pfrm, app, sprite_.get_position());

    room.apply_damage(pfrm, app, 100);
}



} // namespace skyland
