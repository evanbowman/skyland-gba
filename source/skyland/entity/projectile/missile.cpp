#include "missile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



SHARED_VARIABLE(missile_damage);



Missile::Missile(const Vec2<Float>& position,
                 const Vec2<Float>& target,
                 u8 source_x,
                 u8 source_y,
                 Island* source)
    : Projectile({{10, 10}, {8, 8}}), target_x_(target.x), source_(source),
      source_x_(source_x), source_y_(source_y)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(25);

    sprite_.set_origin({8, 16});
}



void Missile::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    if (sprite_.get_position().y < 450) {
        sprite_.set_alpha(Sprite::Alpha::transparent);
    } else {
        sprite_.set_alpha(Sprite::Alpha::opaque);
    }

    timer_ -= delta;

    switch (state_) {
    case State::falling: {
        if (timer_ < 0) {
            timer_ = seconds(2);
            state_ = State::wait;
        }
        auto pos = sprite_.get_position();
        pos.y -= delta * 0.00041f;
        sprite_.set_position(pos);
        break;
    }

    case State::wait:
        if (timer_ < 0) {
            timer_ = milliseconds(400);
            state_ = State::rising;

            auto pos = sprite_.get_position();
            pos.x = (source_x_ * 16) + 8 + source_->origin().x;
            sprite_.set_position(pos);
            sprite_.set_flip({false, false});
        }
        break;

    case State::rising: {
        if (timer_ < 0) {
            if (auto room = source_->get_room({source_x_, source_y_})) {
                // The missile being fully rewound needs to be the factor that
                // triggers a reload in the missile-silo. Otherwise, bugs might
                // arise, where players with quick reflexes might be able to
                // clone projectiles by exploiting inaccuracies in the rewind
                // clock.
                room->___rewind___ability_used(pfrm, app);
            }
            kill();
        }
        auto pos = sprite_.get_position();
        pos.y += delta * 0.0003f;
        sprite_.set_position(pos);
        break;
    }
    }
}



void Missile::update(Platform& pfrm, App& app, Microseconds delta)
{
    timer_ += delta;

    // FIXME: Most entities in the game do not move far enough offscreen for the
    // gba's sprite display wrapping to be a problem. But for this particular
    // missile projectile, I threw in this little hack. Really, we should be
    // creating a bounding box for the view and testing intersection with the
    // missile entity. TODO. Unitl then...
    if (sprite_.get_position().y < 450) {
        sprite_.set_alpha(Sprite::Alpha::transparent);
    } else {
        sprite_.set_alpha(Sprite::Alpha::opaque);
    }

    switch (state_) {
    case State::rising: {
        if (timer_ > milliseconds(400)) {
            timer_ = 0;
            state_ = State::wait;
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
            if (not pfrm.network_peer().is_connected() and
                app.game_mode() not_eq App::GameMode::tutorial) {
                pos.x = rng::sample<5>(pos.x, rng::critical_state);
            }
            sprite_.set_position(pos);
            sprite_.set_flip({false, true});
        }
        break;

    case State::falling:
        if (timer_ > milliseconds(700)) {
            timer_ = 0;
            kill();
        }
        auto pos = sprite_.get_position();
        pos.y += delta * 0.00041f;
        sprite_.set_position(pos);
        break;
    }
}



extern Sound sound_impact;



void Missile::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and room.metaclass() == missile_silo_mt) {
        return;
    }

    if (source_ == room.parent() and room.metaclass() == forcefield_mt) {
        return;
    }

    auto setup_event = [&](time_stream::event::MissileDestroyed& e) {
        e.timer_.set(timer_);
        e.x_pos_.set(sprite_.get_position().x);
        e.y_pos_.set(sprite_.get_position().y);
        e.target_x_.set(target_x_);
        e.source_x_ = source_x_;
        e.source_y_ = source_y_;
        e.state_ = (u8)state_;
    };

    if (source_ == &app.player_island()) {
        time_stream::event::PlayerMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(pfrm, app.level_timer(), e);
    } else {
        time_stream::event::OpponentMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(pfrm, app.level_timer(), e);
    }

    kill();
    app.camera().shake(18);
    big_explosion(pfrm, app, sprite_.get_position());

    auto metac = room.metaclass();

    if (str_cmp((*metac)->name(), "hull") == 0) {
        room.apply_damage(pfrm, app, missile_damage * 0.9f);
    } else {
        room.apply_damage(pfrm, app, missile_damage);
    }

    if (room.health()) {
        sound_impact.play(pfrm, 1);
    }
}



void Missile::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    kill();
    app.camera().shake(18);
    big_explosion(pfrm, app, sprite_.get_position());

    entity.apply_damage(missile_damage);
}



} // namespace skyland
