#pragma once

#include "drone.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/projectile/cannonball.hpp"
#include "skyland/island.hpp"



namespace skyland {



class AttackDrone : public Drone {
public:
    AttackDrone(Island* parent, Island* destination, const Vec2<u8>& grid_pos)
        : Drone(parent, destination, grid_pos)
    {
    }


    const char* name() const
    {
        return "cannon-drone";
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
            update_sprite(app);
            timer_ += delta;
            if (timer_ > milliseconds(3200)) {
                if (target_) {
                    if (auto room = destination()->get_room(*target_)) {

                        auto start = sprite_.get_position();
                        start.x += 8;
                        start.y += 8;
                        auto target = room->center();

                        auto c = alloc_entity<Cannonball>(
                            start, target, parent(), position());
                        if (c) {
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
