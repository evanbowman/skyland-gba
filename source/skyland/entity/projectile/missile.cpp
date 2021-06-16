#include "missile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



Missile::Missile(const Vec2<Float>& position,
                 const Vec2<Float>& target)
    : Projectile({{10, 10}, {8, 8}}),
      target_x_(target.x)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(25);

    sprite_.set_origin({8, 16});
}



void Missile::update(Platform&, App&, Microseconds delta)
{
    timer_ += delta;


    switch (state_) {
    case State::rising: {
        if (timer_ > milliseconds(400)) {
            timer_ = 0;
            state_ = State::wait;
            sprite_.set_alpha(Sprite::Alpha::transparent);
        }
        auto pos = sprite_.get_position();
        pos.y -= delta * 0.0003f;
        sprite_.set_position(pos);
        break;
    }

    case State::wait:
        if (timer_ > seconds(2)) {
            timer_ = 0;
            state_ = State::falling;
            auto pos = sprite_.get_position();
            pos.x = target_x_;
            sprite_.set_position(pos);
            sprite_.set_flip({false, true});
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }
        break;

    case State::falling:
        if (timer_ > seconds(400)) {
            timer_ = 0;
            kill();
        }
        auto pos = sprite_.get_position();
        pos.y += delta * 0.00041f;
        sprite_.set_position(pos);
        break;
    }
}



void Missile::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (state_ not_eq State::falling) {
        return;
    }
    kill();
    app.camera().shake(18);
    big_explosion(pfrm, app, sprite_.get_position());
    room.apply_damage(pfrm, app, Missile::deals_damage);
}







}
