#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/combatDroneSetTargetScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class CombatDrone : public Drone {
public:
    CombatDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
        : Drone(get_name(), parent, destination, grid_pos)
    {
        sprite_.set_texture_index(65);

        health_ = 65;
    }


    static const auto reload_time = milliseconds(3000);


    Microseconds reload_time_remaining() const override
    {
        if (state_ == Drone::State::launch) {
            return reload_time;
        }
        return reload_time - timer_;
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (parent() == &*app.opponent_island()) {
            sprite_.set_texture_index(68);
        }

        switch (state_) {
        case Drone::State::launch:
            Drone::update(pfrm, app, delta);
            break;

        case Drone::State::ready:
            update_sprite(app);
            state_ = State::wait;
            timer_ = 0;
            break;

        case State::wait:
            duration_ += delta;
            update_sprite(app);
            if (timer_ > reload_time) {
                if (target_) {
                    if (not app.opponent_island()) {
                        return;
                    }

                    auto island = target_near_ ? &app.player_island()
                                               : &*app.opponent_island();

                    if (auto drone = island->get_drone(*target_)) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = island->origin();

                        target.x += (*drone)->position().x * 16 + 8;
                        target.y += (*drone)->position().y * 16 + 8;

                        auto c = alloc_entity<Cannonball>(
                            start, target, parent(), position());
                        if (c) {
                            app.camera().shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                        state_ = Drone::State::ready;
                        timer_ = 0;
                    } else {
                        target_.reset();
                    }
                }
            } else {
                timer_ += delta;
            }

            break;
        }
    }


    static Coins cost()
    {
        return 0;
    }


    static const char* get_name()
    {
        return "combat-drone";
    }


    const char* name() const override
    {
        return get_name();
    }


    static u16 icon()
    {
        return 1080;
    }


    static u16 unsel_icon()
    {
        return 1096;
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app) override
    {
        return scene_pool::alloc<CombatDroneSetTargetScene>(shared_from_this());
    }

    enum State : u8 {
        __derived = Drone::State::ready,
        wait,
    };
};



} // namespace skyland
