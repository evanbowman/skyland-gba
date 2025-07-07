////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "genericBird.hpp"
#include "number/random.hpp"
#include "skyland/island.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "skyland/weather/dustStorm.hpp"



namespace skyland
{



GenericBird::GenericBird(Platform::DynamicTexturePtr dt,
                         const RoomCoord& position,
                         bool near)
    : Bird({{}, {}}), dt_(dt), position_(position), near_(near)
{
    // NOTE: Two bird graphics of different color sit interleaved in texture
    // data.
    color_ = rng::choice<2>(rng::utility_state);

    dt_->remap(63 * 2 + color_);

    sprite_.set_texture_index(dt->mapping_index() * 2);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

    // sprite_.set_priority(2);
}



GenericBird::GenericBird(Platform::DynamicTexturePtr dt,
                         const RoomCoord& coord,
                         const Vec2<Fixnum>& position,
                         Float speed,
                         Time flight_timer,
                         u8 color,
                         bool near,
                         bool flip)
    : Bird({{}, {}}), dt_(dt), position_(coord), near_(near),
      flight_timer_(flight_timer), color_(color), speed_(speed)
{
    sprite_.set_position(position);
    sprite_.set_flip({flip, false});
    sprite_.set_size(Sprite::Size::w16_h32);
    // sprite_.set_priority(2);
    sprite_.set_texture_index(dt->mapping_index() * 2);

    anim_index_ = 1;
    dt_->remap((63 + anim_index_) * 2 + color_);

    state_ = State::fly;

    alerted_ = true;
}



void GenericBird::roost(Island* island, Time delta)
{
    auto o = island->origin();
    o.x += Fixnum::from_integer(position_.x * 16);
    o.y += Fixnum::from_integer(position_.y * 16);

    auto layer = island->layer();
    auto t = PLATFORM.get_tile(layer, position_.x, position_.y);
    auto below = PLATFORM.get_tile(layer, position_.x, position_.y + 1);

    if ((below == Tile::null or below == Tile::grass or
         below == Tile::liberty_1 or below == Tile::lava_top or
         below == Tile::lava_left or below == Tile::lava_right or
         below == Tile::balloon_1 or below == Tile::balloon_3) and
        delta > 0) {
        alerted_ = true;
    } else if (below == Tile::null or below == Tile::grass) {
        ++position_.y;
    }

    switch (t) {

    case InteriorTile::roof_1:
    case InteriorTile::roof_3:
        o.y -= 10.0_fixed;
        break;

    case InteriorTile::roof_2:
        // A bird sitting atop a billowing chimney just looks silly.
        this->kill();
        break;

    case InteriorTile::flag_mount:
        o.y -= 32.0_fixed;
        o.x += 5.0_fixed;
        break;

    case InteriorTile::roof_flag:
        o.y -= 32.0_fixed;
        o.x += 5.0_fixed;
        break;

    case InteriorTile::null:
    case InteriorTile::grass:
    case InteriorTile::airborne_selection:
        break;

    default:
        if (delta > 0) {
            alerted_ = true;
        }
        position_.y -= 1;
        break;
    }

    o.y += Fixnum::from_integer(island->get_ambient_movement());

    sprite_.set_position(o);
}



void GenericBird::update(Time delta)
{
    Island* island = nullptr;

    if (near_) {
        island = &player_island();
    } else {
        island = opponent_island();
    }

    if (island == nullptr) {
        this->kill();
        return;
    }

    if (island->is_destroyed()) {
        this->signal();
    }

    if (island not_eq nullptr) {
        for (auto& c : island->outdoor_characters()) {
            if (c->grid_position() == coordinate()) {
                alerted_ = true;
            }
        }
    }

    switch (state_) {
    case State::roost: {

        roost(island, delta);

        if (alerted_ and delta > 0) {
            state_ = State::fly;
            anim_timer_ = 0;

            sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

            speed_ = 0.00003f * (1 + rng::choice<3>(rng::utility_state));
        }
        break;
    }

    case State::caw: {
        roost(island, delta);

        anim_timer_ += delta;
        flight_timer_ += delta;
        if (anim_timer_ > milliseconds(150)) {
            anim_timer_ -= milliseconds(150);
            anim_index_ = not anim_index_;
            dt_->remap((75 + anim_index_) * 2 + color_);
        }
        if (flight_timer_ > seconds(2)) {
            flight_timer_ = 0;
            dt_->remap(63 * 2 + color_);
            state_ = State::roost;
        }

        if (alerted_ and delta > 0) {
            state_ = State::fly;
            anim_timer_ = 0;
            flight_timer_ = 0;
            sprite_.set_flip({(bool)rng::choice<2>(rng::utility_state), false});

            speed_ = 0.00003f * (1 + rng::choice<3>(rng::utility_state));
        }

        break;
    }

    case State::fly: {
        anim_timer_ += delta;
        flight_timer_ += delta;
        if (anim_timer_ > milliseconds(110)) {
            anim_timer_ -= milliseconds(110);

            if (anim_index_ == 6) {
                anim_index_ = 1;
            } else {
                ++anim_index_;
            }

            dt_->remap((63 + anim_index_) * 2 + color_);
        }

        auto pos = sprite_.get_position();

        if (pos.y < 450.0_fixed) {

            if (alive()) {
                time_stream::event::BirdLeftMap e;
                e.x_coord_ = position_.x;
                e.y_coord_ = position_.y;
                e.x_pos_.set(sprite_.get_position().x.as_integer());
                e.y_pos_.set(sprite_.get_position().y.as_integer());
                memcpy(e.speed_, &speed_, sizeof(speed_));
                e.flight_timer_.set(flight_timer_);
                e.color_ = color_;
                e.flip_ = sprite_.get_flip().x;
                e.near_ = near_;
                APP.push_time_stream(e);
            }

            kill();
        }

        if (delta > 0) {
            pos.y -= Fixnum::from_integer(delta) * 0.0001_fixed;
        }

        if (sprite_.get_flip().x) {
            pos.x += Fixnum::from_integer(delta) * Fixnum(speed_);
        } else {
            pos.x -= Fixnum::from_integer(delta) * Fixnum(speed_);
        }

        sprite_.set_position(pos);

        break;
    }
    }
}



void GenericBird::rewind(Time delta)
{
    Island* island = nullptr;

    if (near_) {
        island = &player_island();
    } else {
        island = opponent_island();
    }

    if (island->is_destroyed()) {
        this->signal();
    }

    if (island == nullptr) {
        this->kill();
    }

    switch (state_) {
    case State::roost: {
        roost(island, 0);
        break;
    }

    case State::caw: {
        flight_timer_ = 0;
        anim_timer_ = 0;
        dt_->remap(63 * 2 + color_);
        state_ = State::roost;
        break;
    }

    case State::fly: {
        flight_timer_ -= delta;
        if (flight_timer_ <= 0) {
            state_ = State::roost;
            alerted_ = false;
            dt_->remap(63 * 2 + color_);
            sprite_.set_texture_index(dt_->mapping_index() * 2);
            sprite_.set_size(Sprite::Size::w16_h32);
            break;
        }
        anim_timer_ -= delta;
        if (anim_timer_ < 0) {
            anim_timer_ += milliseconds(110);

            if (anim_index_ == 1) {
                anim_index_ = 6;
            } else {
                --anim_index_;
            }

            dt_->remap((63 + anim_index_) * 2 + color_);
        }

        auto pos = sprite_.get_position();

        if (delta > 0) {
            pos.y += Fixnum(delta * 0.0001f);
        }

        if (sprite_.get_flip().x) {
            pos.x -= Fixnum(delta * speed_);
        } else {
            pos.x += Fixnum(delta * speed_);
        }

        sprite_.set_position(pos);

        break;
    }
    }
}



void GenericBird::signal()
{
    if (state_ == State::roost) {
        alerted_ = true;
    }
}



Island* GenericBird::island()
{
    if (near_) {
        return &player_island();
    } else {
        return opponent_island();
    }
}



void GenericBird::generate()
{
    if (APP.environment().id() == weather::DustStorm::id_) {
        return;
    }

    for (auto it = APP.birds().begin(); it not_eq APP.birds().end();) {
        if ((*it)->island() == opponent_island()) {
            it = APP.birds().erase(it);
        } else {
            ++it;
        }
    }

    static const int max_birds = 6;
    int remaining_birds = max_birds;
    if (rng::choice<4>(rng::utility_state) > 0) {
        int used = rng::choice<4>(rng::utility_state);
        remaining_birds -= used;
        GenericBird::spawn(*APP.opponent_island(), used);
    }
    if (rng::choice<4>(rng::utility_state) > 0) {
        GenericBird::spawn(APP.player_island(),
                           rng::choice(remaining_birds, rng::utility_state));
    }
}



void GenericBird::spawn(Island& island, int count)
{
    Buffer<u8, 10> used;

    for (auto& bird : APP.birds()) {
        if (bird->island() == &island) {
            used.push_back(bird->coordinate().x);
        }
    }

    for (int i = 0; i < count; ++i) {
        u8 tries = 40;
        u8 column = 0;
        bool retry = false;
        while (tries--) {
            column = rng::choice(island.terrain().size(), rng::utility_state);
            for (auto& u : used) {
                if (column == u) {
                    retry = true;
                    break;
                }
            }
            if (not retry) {
                used.push_back(column);
                break;
            }
        }

        if (retry) {
            continue;
        }

        for (u8 y = 0; y < 15; ++y) {
            if (y == 14 or island.rooms_plot().get(column, y + 1)) {

                auto pos = RoomCoord{column, y};

                if (auto dt = PLATFORM.make_dynamic_texture()) {
                    bool near = is_player_island(&island);
                    APP.birds().push(
                        APP.alloc_entity<GenericBird>(*dt, pos, near));
                    break;
                }
            }
        }
    }
}



} // namespace skyland
