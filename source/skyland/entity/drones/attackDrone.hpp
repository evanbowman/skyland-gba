#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland {



extern Sound cannon_sound;



class AttackDrone : public Drone {
public:
    AttackDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
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


    ScenePtr<Scene> select(Platform& pfrm, App& app) override
    {
        pfrm.speaker().play_sound("drone_beep", 1);
        return scene_pool::alloc<WeaponSetTargetScene>(
            position(), destination() == &app.player_island());
    }


    static const auto reload_time = milliseconds(3200);


    Microseconds reload_time_remaining() const override
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


    void ___rewind___ability_used(Platform& pfrm, App& app) override
    {
        if (state_ not_eq Drone::State::launch) {
            timer_ = reload_time;
        }
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (parent() == &*app.opponent_island()) {
            sprite_.set_texture_index(67);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(pfrm, app, delta);
            break;

        case Drone::State::ready:
            update_sprite(pfrm, app);
            state_ = State::wait;
            timer_ = 0;
            break;

        case State::wait:
            duration_ += delta;
            update_sprite(pfrm, app);
            if (timer_ > reload_time) {
                if (target_) {
                    if (not app.opponent_island()) {
                        return;
                    }

                    Island* target_island;
                    if (parent() == &app.player_island()) {
                        target_island = &*app.opponent_island();
                    } else {
                        target_island = &app.player_island();
                    }

                    if (auto room = target_island->get_room(*target_)) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = room->center();

                        cannon_sound.play(pfrm, 3);

                        auto c = app.alloc_entity<Cannonball>(
                            pfrm, start, target, parent(), position());
                        if (c) {
                            app.camera().shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                        timer_ = 0;
                        state_ = Drone::State::ready;
                    } else {
                        target_.reset();
                    }
                }
            } else {
                timer_ += delta;

                if (timer_ > reload_time) {
                    time_stream::event::DroneReloadComplete e;
                    e.x_pos_ = position().x;
                    e.y_pos_ = position().y;
                    e.destination_near_ = destination() == &app.player_island();
                    app.time_stream().push(pfrm, app.level_timer(), e);
                }
            }

            break;
        }
    }

    enum State : u8 {
        __derived = Drone::State::ready,
        wait,
    };
};



} // namespace skyland
