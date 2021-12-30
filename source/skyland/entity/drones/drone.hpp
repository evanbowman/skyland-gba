#pragma once

#include "memory/rc.hpp"
#include "skyland/entity.hpp"



namespace skyland {



class Island;



class Drone : public Entity, public IntrusiveRcControlBlock<Drone> {
public:
    Drone(Island* parent,
          Island* destination,
          const Vec2<u8>& grid_pos);


    void update(Platform&, App&, Microseconds delta) override;


    const Vec2<u8>& position() const
    {
        return grid_pos_;
    }


    void set_movement_target(const Vec2<u8>& position)
    {
        movement_target_ = position;
    }


    void set_target(const Vec2<u8>& target)
    {
        target_ = target;
    }

protected:
    enum class State {
        launch,
        ready,
    } state_ = State::launch;

private:
    Island* parent_;
    Island* destination_;
    Vec2<u8> grid_pos_;
    Vec2<u8> movement_target_;
    std::optional<Vec2<u8>> target_;

    Vec2<Float> anchor_;

    Microseconds timer_ = 0;
};



} // namespace skyland
