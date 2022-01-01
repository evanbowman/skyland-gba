#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/combatDroneSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/entity/projectile/cannonball.hpp"



namespace skyland {



class CombatDrone : public Drone {
public:

    CombatDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
        : Drone(parent, destination, grid_pos)
    {
        sprite_.set_texture_index(65);

        health_ = 65;
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
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
            if (timer_ > milliseconds(3200)) {
                if (target_) {
                    if (not app.opponent_island()) {
                        return;
                    }

                    auto island = target_near_ ?
                        &app.player_island() :
                        &*app.opponent_island();

                    if (auto drone = island->get_drone(*target_)) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = island->origin();

                        target.x += (*drone)->position().x * 16;
                        target.y += (*drone)->position().y * 16;

                        auto c = alloc_entity<Cannonball>(
                            start, target, parent(), position());
                        if (c) {
                            app.camera().shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                    }
                    timer_ = 0;
                    state_ = Drone::State::ready;
                }
            } else {
                timer_ += delta;
            }

            break;
        }
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
        // TODO...
        return 512;
    }


    static u16 unsel_icon()
    {
        // TODO...
        return 512;
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
