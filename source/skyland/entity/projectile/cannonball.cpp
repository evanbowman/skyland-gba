#include "projectile.hpp"


#include "cannonball.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"



namespace skyland {



Cannonball::Cannonball(const Vec2<Float>& position,
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



void Cannonball::update(Platform&, App&, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + Float(delta) * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (timer_ > seconds(1)) {
        kill();
    }
}



void Cannonball::on_collision(Platform& pfrm, App& app, Room& room)
{
    // Argh. I know, I shouldn't be doing this.
    if (source_ == room.parent() and dynamic_cast<Cannon*>(&room)) {
        return;
    }

    if (source_ == room.parent() and dynamic_cast<Forcefield*>(&room)) {
        return;
    }


    kill();
    app.camera().shake(8);
    medium_explosion(pfrm, app, sprite_.get_position());

    room.apply_damage(pfrm, app, 40);
}



} // namespace skyland
