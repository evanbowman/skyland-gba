#include "missile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



Missile::Missile(const Vec2<Float>& position,
                 const Vec2<Float>& target,
                 Island* source)
    : Projectile({{10, 10}, {8, 8}}), target_x_(target.x), source_(source)
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
            pos.x = rng::sample<5>(pos.x, rng::utility_state);
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
    if (source_ == room.parent() and dynamic_cast<MissileSilo*>(&room)) {
        return;
    }

    if (source_ == room.parent() and dynamic_cast<Forcefield*>(&room)) {
        return;
    }

    kill();
    app.camera().shake(18);
    big_explosion(pfrm, app, sprite_.get_position());

    auto metac = room.metaclass();

    if (str_cmp((*metac)->name(), "hull") == 0) {
        room.apply_damage(pfrm, app, Missile::deals_damage * 0.75f);
    } else {
        room.apply_damage(pfrm, app, Missile::deals_damage);
    }
}



} // namespace skyland
