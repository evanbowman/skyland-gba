////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "missile.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/entity/misc/smokePuff.hpp"
#include "skyland/entity/projectile/flak.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/rooms/missileSilo.hpp"
#include "skyland/rooms/rocketSilo.hpp"
#include "skyland/rooms/warhead.hpp"
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



Fixnum Missile::fall_speed()
{
    if (PLATFORM.network_peer().is_connected()) {
        return 0.00034_fixed;
    } else {
        return 0.00041_fixed;
    }
}



void Missile::rewind(Time delta)
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
        pos.y -= APP.delta_fp() * fall_speed();
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
                room->___rewind___ability_used();
            }
            kill();
        }
        auto pos = sprite_.get_position();
        pos.y += APP.delta_fp() * 0.0003_fixed;
        sprite_.set_position(pos);
        break;
    }
    }
}



void Missile::update(Time delta)
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
        pos.y -= APP.delta_fp() * 0.0003_fixed;
        sprite_.set_position(pos);
        break;
    }

    case State::wait:
        if (timer_ > seconds(2)) {
            timer_ = 0;
            state_ = State::falling;
            auto pos = sprite_.get_position();
            pos.x = target_x_;
            if (not PLATFORM.network_peer().is_connected() and
                APP.game_mode() not_eq App::GameMode::tutorial) {
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
        pos.y += APP.delta_fp() * fall_speed();
        sprite_.set_position(pos);

        Island* target;
        if (is_player_island(source_)) {
            target = APP.opponent_island();
        } else {
            target = &APP.player_island();
        }

        if (target) {
            auto max_y = target->origin().y;
            max_y += Fixnum::from_integer(16 * 16 + 32);
            if (pos.y > max_y) {
                this->on_destroy();
                PLATFORM.speaker().play_sound("explosion1", 2);
            }
        }
        break;
    }
    }
}



extern Sound sound_impact;



void Missile::on_destroy()
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

    if (is_player_island(source_)) {
        time_stream::event::PlayerMissileDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentMissileDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    }

    kill();
    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());
}



void Missile::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and room.metaclass() == missile_silo_mt) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    on_destroy();

    auto metac = room.metaclass();

    if (str_cmp((*metac)->name(), "hull") == 0) {
        room.apply_damage(missile_damage * 0.9f);
    } else if (str_cmp((*metac)->name(), "stacked-hull") == 0) {
        room.apply_damage(missile_damage / 4);
    } else {
        room.apply_damage(missile_damage);
    }

    if (room.health()) {
        sound_impact.play(1);
    }
}



void Missile::on_collision(Entity& entity)
{
    bool skip_destroy = false;

    if (auto drone = entity.cast_drone()) {
        if (drone->ignores_damage()) {
            skip_destroy = true;
        }
    }

    if (not skip_destroy) {
        kill();
    }

    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());

    entity.apply_damage(missile_damage);
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


    void update(Time delta) override
    {
        if (delta == 0) {
            return;
        }

        auto pos = sprite_.get_position();
        pos = pos + speed_ * APP.delta_fp();
        sprite_.set_position(pos);

        if (APP.game_speed() == GameSpeed::fast) {
            speed_.y = speed_.y + 0.00002_fixed;
        } else if (APP.game_speed() == GameSpeed::slow) {
            speed_.y = speed_.y + 0.000005_fixed;
        } else {
            speed_.y = speed_.y + 0.00001_fixed;
        }


        auto max_y = parent_->origin().y;
        max_y += Fixnum::from_integer(16 * 16 + 32);
        if (pos.y > max_y) {
            kill();
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }


    void on_collision(Room& room, Vec2<u8> origin) override
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

        room.apply_damage(damage);
        APP.camera()->shake(4);

        sound_impact.play(1);
        medium_explosion(sprite_.get_position());

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



SHARED_VARIABLE(rocket_bomb_impact_damage);
SHARED_VARIABLE(rocket_bomb_splash_damage);



void RocketBomb::burst(const Vec2<Fixnum>& position, Room& origin_room)
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
                    room->apply_damage(damage / 4);
                } else {
                    room->apply_damage(damage);
                }
                origin_room.parent()->fire_create(Vec2<u8>{(u8)x, (u8)y});
            }
        }
    };

    apply_damage(0, 0, rocket_bomb_impact_damage);

    apply_damage(1, 0, rocket_bomb_splash_damage);
    apply_damage(-1, 0, rocket_bomb_splash_damage);
    apply_damage(0, 1, rocket_bomb_splash_damage);
    apply_damage(0, -1, rocket_bomb_splash_damage);
}



void RocketBomb::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and str_eq(room.name(), RocketSilo::name())) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }


    burst(sprite_.get_position(), room);

    on_destroy();


    if (room.health()) {
        sound_impact.play(1);
    }
}



void RocketBomb::on_destroy()
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

    if (is_player_island(source_)) {
        time_stream::event::PlayerRocketBombDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRocketBombDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    }

    make_flak_smoke(sprite_.get_position());
    make_flak_smoke(sprite_.get_position());

    kill();
    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());
}



void ClumpMissile::burst(const Vec2<Fixnum>& position, Room& origin_room)
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
                    room->apply_damage(damage / 4);
                } else {
                    room->apply_damage(damage);
                }
            }
        }
    };

    apply_damage(0, 0, 40);

    apply_damage(1, 0, 16);
    apply_damage(-1, 0, 16);
    apply_damage(0, 1, 16);
    apply_damage(0, -1, 16);

    spawn_bomblets(origin_room.parent(), sprite_.get_position());
}



void ClumpMissile::spawn_bomblets(Island* source, Vec2<Fixnum> origin)
{
    for (int i = 0; i < 10; ++i) {
        if (auto b = alloc_entity<Bomblet>(source, origin)) {
            Vec2<Float> dirv = {0, -1};
            if (rng::choice<2>(rng::critical_state)) {
                dirv = rotate(dirv, rng::choice<35>(rng::critical_state));
            } else {
                dirv = rotate(dirv, rng::choice<360 - 35>(rng::critical_state));
            }

            Fixnum add_speed = 1.0_fixed + 0.05_fixed * Fixnum(i);

            b->set_vector(Fixnum(dirv.x) * 0.0001_fixed * add_speed,
                          Fixnum(dirv.y) * 0.0002_fixed * add_speed);
            source->projectiles().push(std::move(b));
        }
    }
}



void ClumpMissile::on_collision(Entity& entity)
{
    bool skip_destroy = false;

    if (auto drone = entity.cast_drone()) {
        if (drone->ignores_damage()) {
            skip_destroy = true;
        }
    }

    if (not skip_destroy) {
        kill();
    }

    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());

    entity.apply_damage(missile_damage);

    spawn_bomblets(source_, sprite_.get_position());
}



void ClumpMissile::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and str_eq(room.name(), RocketSilo::name())) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }


    burst(sprite_.get_position(), room);

    on_destroy();


    if (room.health()) {
        sound_impact.play(1);
    }
}



void ClumpMissile::on_destroy()
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

    if (is_player_island(source_)) {
        time_stream::event::PlayerClumpMissileDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentClumpMissileDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    }

    make_flak_smoke(sprite_.get_position());
    make_flak_smoke(sprite_.get_position());

    kill();
    APP.camera()->shake(18);
    big_explosion(sprite_.get_position());
}



void AtomicMissile::burst(const Vec2<Fixnum>& position, Room& origin_room)
{
    int grid_x_start = origin_room.position().x;
    int grid_y_start = origin_room.position().y;

    auto apply_damage = [&](int x_off, int y_off, Health damage) {
        auto island = origin_room.parent();
        const int x = grid_x_start + x_off;
        const int y = grid_y_start + y_off;
        if (x >= 0 and x < 16 and y >= 0 and y < 16) {
            if (auto room = island->get_room({u8(x), u8(y)})) {
                if ((*room->metaclass())->properties() &
                    RoomProperties::habitable) {
                    room->apply_damage(12);
                } else {
                    room->apply_damage(damage);
                }
                origin_room.parent()->fire_create(Vec2<u8>{(u8)x, (u8)y});
            }
            if (auto chr = island->character_at_location({(u8)x, (u8)y})) {
                chr->apply_radiation_damage(50);
            }
        }
    };

    apply_damage(0, 0, 500);

    apply_damage(1, 0, 150);
    apply_damage(-1, 0, 150);
    apply_damage(0, 1, 150);
    apply_damage(0, -1, 150);

    for (int x = -5; x < 6; ++x) {
        for (int y = -5; y < 6; ++y) {
            apply_damage(x, y, 32);
        }
    }

    PLATFORM.speaker().play_sound("explosion2", 8);
}



void AtomicMissile::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent() and str_eq(room.name(), Warhead::name())) {
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < missile_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    auto pos = sprite_.get_position();
    pos.y += 4.0_fixed;
    ExploSpawner::create(pos);

    ExploSpawner::create(rng::sample<48>(pos, rng::utility_state));
    ExploSpawner::create(rng::sample<48>(pos, rng::utility_state));



    burst(sprite_.get_position(), room);

    on_destroy();


    if (room.health()) {
        sound_impact.play(1);
    }
}



void AtomicMissile::on_destroy()
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

    if (is_player_island(source_)) {
        time_stream::event::PlayerAtomicDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentAtomicDestroyed e;
        setup_event(e);
        APP.time_stream().push(APP.level_timer(), e);
    }


    kill();
    APP.camera()->shake(32);
    big_explosion(sprite_.get_position(),
                  BigExplosionConfig{.centerflash_ = true});
}



} // namespace skyland
