////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "missile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/rocketSilo.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



extern SharedVariable flak_r1_damage;
extern SharedVariable flak_r2_damage;
extern SharedVariable flak_r3_damage;

SHARED_VARIABLE(missile_damage);



Missile::Missile(const Vec2<Fixnum>& position,
                 const Vec2<Fixnum>& target,
                 u8 source_x,
                 u8 source_y,
                 Island* source)
    : Projectile({{10, 26}, {8, 16}}), target_x_(target.x), source_(source),
      source_x_(source_x), source_y_(source_y)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(25);

    sprite_.set_origin({8, 16});
}



Fixnum Missile::fall_speed(Platform& pfrm)
{
    if (pfrm.network_peer().is_connected()) {
        return 0.00034_fixed;
    } else {
        return 0.00041_fixed;
    }
}



void Missile::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    if (sprite_.get_position().y < 450.0_fixed) {
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
        pos.y -= app.delta_fp() * fall_speed(pfrm);
        sprite_.set_position(pos);
        break;
    }

    case State::wait:
        if (timer_ < 0) {
            timer_ = milliseconds(400);
            state_ = State::rising;

            auto pos = sprite_.get_position();
            pos.x = Fixnum::from_integer((source_x_ * 16) + 8) +
                    source_->origin().x;
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
        pos.y += app.delta_fp() * 0.0003_fixed;
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

    switch (state_) {
    case State::rising: {
        if (timer_ > milliseconds(400)) {
            timer_ = 0;
            state_ = State::wait;
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else if (sprite_.get_position().y < 460.0_fixed) {
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else {
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }

        auto pos = sprite_.get_position();
        pos.y -= app.delta_fp() * 0.0003_fixed;
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
                pos.x = rng::sample<3>(pos.x, rng::critical_state);
            }
            sprite_.set_position(pos);
            sprite_.set_flip({false, true});
        }
        break;

    case State::falling: {
        if (sprite_.get_position().y < 460.0_fixed) {
            sprite_.set_alpha(Sprite::Alpha::transparent);
        } else {
            sprite_.set_alpha(Sprite::Alpha::opaque);
        }
        if (timer_ > milliseconds(700)) {
            timer_ = 0;
            kill();
        }
        auto pos = sprite_.get_position();
        pos.y += app.delta_fp() * fall_speed(pfrm);
        sprite_.set_position(pos);

        Island* target;
        if (source_ == &app.player_island()) {
            target = app.opponent_island();
        } else {
            target = &app.player_island();
        }

        if (target) {
            auto max_y = target->origin().y;
            max_y += Fixnum::from_integer(16 * 16 + 32);
            if (pos.y > max_y) {
                this->destroy(pfrm, app);
                pfrm.speaker().play_sound("explosion1", 2);
            }
        }
        break;
    }
    }
}



extern Sound sound_impact;



void Missile::destroy(Platform& pfrm, App& app)
{
    auto setup_event = [&](time_stream::event::MissileDestroyed& e) {
        e.timer_.set(timer_);
        e.x_pos_.set(sprite_.get_position().x.as_integer());
        e.y_pos_.set(sprite_.get_position().y.as_integer());
        e.target_x_.set(target_x_.as_integer());
        e.source_x_ = source_x_;
        e.source_y_ = source_y_;
        e.state_ = (u8)state_;
    };

    if (source_ == &app.player_island()) {
        time_stream::event::PlayerMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    } else {
        time_stream::event::OpponentMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    }

    kill();
    app.camera()->shake(18);
    big_explosion(pfrm, app, sprite_.get_position());
}



void Missile::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and room.metaclass() == missile_silo_mt) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    destroy(pfrm, app);

    auto metac = room.metaclass();

    if (str_cmp((*metac)->name(), "hull") == 0) {
        room.apply_damage(pfrm, app, missile_damage * 0.9f, source_);
    } else if (str_cmp((*metac)->name(), "stacked-hull") == 0) {
        room.apply_damage(pfrm, app, missile_damage / 4, source_);
    } else {
        room.apply_damage(pfrm, app, missile_damage, source_);
    }

    if (room.health()) {
        sound_impact.play(pfrm, 1);
    }
}



void Missile::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    kill();
    app.camera()->shake(18);
    big_explosion(pfrm, app, sprite_.get_position());

    entity.apply_damage(pfrm, app, missile_damage);
}



class Bomblet : public Projectile
{
public:
    Bomblet(Island* parent, const Vec2<Fixnum>& position)
        : Projectile({{6, 6}, {3, 3}}), parent_(parent)
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w8_h8);
        sprite_.set_texture_index(89 * 8 + 2);
        sprite_.set_origin({4, 4});
    }


    void set_vector(Fixnum hspeed, Fixnum vspeed)
    {
        speed_ = {hspeed, vspeed};
    }


    void update(Platform& pfrm, App& app, Microseconds delta)
    {
        if (delta == 0) {
            return;
        }

        auto pos = sprite_.get_position();
        pos = pos + speed_ * app.delta_fp();
        sprite_.set_position(pos);

        speed_.y = speed_.y + 0.00001_fixed;

        auto max_y = parent_->origin().y;
        max_y += Fixnum::from_integer(16 * 16 + 32);
        if (pos.y > max_y) {
            kill();
        }
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        kill();
    }


    void on_collision(Platform& pfrm, App& app, Room& room) override
    {
        int damage = 25;
        switch (bounce_) {
        case 1:
            damage = 10;
            break;

        case 0:
            damage = 5;
            break;
        }

        room.apply_damage(pfrm, app, damage, parent_);
        app.camera()->shake(4);

        sound_impact.play(pfrm, 1);
        medium_explosion(pfrm, app, sprite_.get_position());

        if (bounce_) {
            --bounce_;
            speed_.y *= Fixnum(-0.5f);
        } else {
            kill();
        }
    }


private:
    Vec2<Fixnum> speed_;
    Island* parent_;
    int bounce_ = 2;
};



void RocketBomb::burst(Platform& pfrm,
                       App& app,
                       const Vec2<Fixnum>& position,
                       Room& origin_room)
{
    int grid_x_start = origin_room.position().x;
    int grid_y_start = origin_room.position().y;

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = origin_room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto room = island->get_room({u8(x), u8(y)})) {
                if (str_cmp(room->name(), "stacked-hull") == 0) {
                    room->apply_damage(pfrm, app, damage / 4);
                } else {
                    room->apply_damage(pfrm, app, damage);
                }
                origin_room.parent()->fire_create(
                    pfrm, app, Vec2<u8>{(u8)x, (u8)y});
            }
        }
    };

    apply_damage(0, 0, 40);

    apply_damage(1, 0, 16);
    apply_damage(-1, 0, 16);
    apply_damage(0, 1, 16);
    apply_damage(0, -1, 16);
}



void RocketBomb::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and str_eq(room.name(), RocketSilo::name())) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }


    burst(pfrm, app, sprite_.get_position(), room);

    destroy(pfrm, app);


    if (room.health()) {
        sound_impact.play(pfrm, 1);
    }
}



void RocketBomb::destroy(Platform& pfrm, App& app)
{
    auto setup_event = [&](time_stream::event::MissileDestroyed& e) {
        e.timer_.set(timer_);
        e.x_pos_.set(sprite_.get_position().x.as_integer());
        e.y_pos_.set(sprite_.get_position().y.as_integer());
        e.target_x_.set(target_x_.as_integer());
        e.source_x_ = source_x_;
        e.source_y_ = source_y_;
        e.state_ = (u8)state_;
    };

    if (source_ == &app.player_island()) {
        time_stream::event::PlayerRocketBombDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    } else {
        time_stream::event::OpponentRocketBombDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    }

    auto flak_smoke = [](Platform& pfrm, App& app, const Vec2<Fixnum>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            pfrm, rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(pfrm, app, sprite_.get_position());
    flak_smoke(pfrm, app, sprite_.get_position());

    kill();
    app.camera()->shake(18);
    big_explosion(pfrm, app, sprite_.get_position());
}



void ClumpMissile::burst(Platform& pfrm,
                         App& app,
                         const Vec2<Fixnum>& position,
                         Room& origin_room)
{
    int grid_x_start = origin_room.position().x;
    int grid_y_start = origin_room.position().y;

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = origin_room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto room = island->get_room({u8(x), u8(y)})) {
                if (str_cmp(room->name(), "stacked-hull") == 0) {
                    room->apply_damage(pfrm, app, damage / 4);
                } else {
                    room->apply_damage(pfrm, app, damage);
                }
            }
        }
    };

    apply_damage(0, 0, 40);

    apply_damage(1, 0, 16);
    apply_damage(-1, 0, 16);
    apply_damage(0, 1, 16);
    apply_damage(0, -1, 16);

    for (int i = 0; i < 10; ++i) {
        if (auto b = alloc_entity<Bomblet>(origin_room.parent(),
                                           sprite_.get_position())) {
            Vec2<Float> dirv = {0, -1};
            if (rng::choice<2>(rng::critical_state)) {
                dirv = rotate(dirv, rng::choice<35>(rng::critical_state));
            } else {
                dirv = rotate(dirv, rng::choice<360 - 35>(rng::critical_state));
            }

            Fixnum add_speed = 1.0_fixed + 0.05_fixed * Fixnum(i);

            b->set_vector(Fixnum(dirv.x) * 0.0001_fixed * add_speed,
                          Fixnum(dirv.y) * 0.0002_fixed * add_speed);
            origin_room.parent()->projectiles().push(std::move(b));
        }
    }
}



void ClumpMissile::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent() and str_eq(room.name(), RocketSilo::name())) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }


    burst(pfrm, app, sprite_.get_position(), room);

    destroy(pfrm, app);


    if (room.health()) {
        sound_impact.play(pfrm, 1);
    }
}



void ClumpMissile::destroy(Platform& pfrm, App& app)
{
    auto setup_event = [&](time_stream::event::MissileDestroyed& e) {
        e.timer_.set(timer_);
        e.x_pos_.set(sprite_.get_position().x.as_integer());
        e.y_pos_.set(sprite_.get_position().y.as_integer());
        e.target_x_.set(target_x_.as_integer());
        e.source_x_ = source_x_;
        e.source_y_ = source_y_;
        e.state_ = (u8)state_;
    };

    if (source_ == &app.player_island()) {
        time_stream::event::PlayerClumpMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    } else {
        time_stream::event::OpponentClumpMissileDestroyed e;
        setup_event(e);
        app.time_stream().push(app.level_timer(), e);
    }

    auto flak_smoke = [](Platform& pfrm, App& app, const Vec2<Fixnum>& pos) {
        auto e = app.alloc_entity<SmokePuff>(
            pfrm, rng::sample<48>(pos, rng::utility_state), 61);

        if (e) {
            app.effects().push(std::move(e));
        }
    };

    flak_smoke(pfrm, app, sprite_.get_position());
    flak_smoke(pfrm, app, sprite_.get_position());

    kill();
    app.camera()->shake(18);
    big_explosion(pfrm, app, sprite_.get_position());
}



} // namespace skyland
