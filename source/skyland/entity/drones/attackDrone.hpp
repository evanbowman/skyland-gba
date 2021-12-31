#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"



namespace skyland {



class AttackDrone : public Drone {
public:
    AttackDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
        : Drone(parent, destination, grid_pos)
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
        return 512;
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app) override
    {
        return scene_pool::alloc<WeaponSetTargetScene>(position(),
                                                       destination() == &app.player_island());
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
            timer_ += delta;
            if (timer_ > milliseconds(3200)) {
                if (target_) {
                    if (not app.opponent_island()) {
                        return;
                    }
                    if (auto room = app.opponent_island()->get_room(*target_)) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = room->center();

                        auto c = alloc_entity<Cannonball>(
                            start, target, parent(), position());
                        if (c) {
                            app.camera().shake(4);
                            parent()->projectiles().push(std::move(c));
                        }
                    }
                }
                timer_ = 0;
                state_ = Drone::State::ready;
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
