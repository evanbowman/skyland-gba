////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



extern Sound cannon_sound;



class AttackDrone : public Drone
{
public:
    AttackDrone(Island* parent, Island* destination, const RoomCoord& grid_pos)
        : Drone(get_name(), parent, destination, grid_pos)
    {
    }


    static const char* get_name()
    {
        return "cannon-drone";
    }


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        // TODO...
        return 1048;
    }


    static u16 unsel_icon()
    {
        // TODO...
        return 1064;
    }


    ScenePtr<Scene> select() override
    {
        PLATFORM.speaker().play_sound("drone_beep", 1);

        Optional<RoomCoord> initial_pos;
        if (target_near_ == (is_player_island(destination()))) {
            initial_pos = target_;
        }

        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), is_player_island(destination()), initial_pos);
    }


    static const auto reload_time = milliseconds(3200);


    Time reload_time_remaining() const override
    {
        if (state_ == Drone::State::launch) {
            return reload_time;
        }
        return reload_time - timer_;
    }


    static Coins cost()
    {
        return 0;
    }


    void ___rewind___ability_used() override
    {
        if (state_ not_eq Drone::State::launch) {
            timer_ = reload_time;
        }
    }


    void update(Time delta) override
    {
        if (parent() == APP.opponent_island()) {
            sprite_.set_texture_index(67);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(delta);
            break;

        case Drone::State::ready:
            update_sprite();
            state_ = State::wait;
            timer_ = 0;
            break;

        case State::wait:
            duration_ += delta;
            update_sprite();
            if (timer_ > reload_time) {
                if (target_) {
                    if (not APP.opponent_island()) {
                        return;
                    }

                    Island* target_island;
                    if (is_player_island(parent())) {
                        target_island = APP.opponent_island();
                    } else {
                        target_island = &APP.player_island();
                    }

                    if (target_) {

                        auto start = sprite_.get_position();
                        start.x += 8.0_fixed;
                        start.y += 8.0_fixed;
                        auto target = target_island->origin();
                        target.x += Fixnum::from_integer(target_->x * 16 + 8);
                        target.y += Fixnum::from_integer(target_->y * 16 + 8);

                        cannon_sound.play(3);

                        auto c = APP.alloc_entity<Cannonball>(
                            start, target, parent(), position());
                        if (c) {
                            APP.camera()->shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                        timer_ = 0;
                        state_ = Drone::State::ready;
                    }
                }
            } else {
                timer_ += delta;

                if (timer_ > reload_time) {
                    time_stream::event::DroneReloadComplete e;
                    e.x_pos_ = position().x;
                    e.y_pos_ = position().y;
                    e.destination_near_ = is_player_island(destination());
                    APP.time_stream().push(APP.level_timer(), e);
                }
            }

            break;
        }
    }


    void display_on_hover(Platform::Screen& screen,

                          const RoomCoord& cursor)
    {
        if (not target_) {
            return;
        }

        Island* target_island;
        if (is_player_island(parent())) {
            target_island = APP.opponent_island();
        } else {
            target_island = &APP.player_island();
        }

        if (target_island) {
            auto pos = target_island->visual_origin();
            pos.x += Fixnum::from_integer(target_->x * 16);
            pos.y += Fixnum::from_integer(target_->y * 16);

            Sprite spr;
            spr.set_position(pos);
            spr.set_texture_index(45);
            spr.set_size(Sprite::Size::w16_h32);

            screen.draw(spr);
        }
    }


    enum State : u8 {
        __derived = Drone::State::ready,
        wait,
    };
};



} // namespace skyland
