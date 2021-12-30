#pragma once

#include "memory/rc.hpp"
#include "skyland/entity.hpp"



namespace skyland {



class Island;



class Drone : public Entity, public IntrusiveRcControlBlock<Drone> {
public:
    Drone(Island* parent, Island* destination, const Vec2<u8>& grid_pos);


    void update(Platform&, App&, Microseconds delta) override;


    const Vec2<u8>& position() const
    {
        return grid_pos_;
    }


    void set_movement_target(const Vec2<u8>& position);



    void set_target(const Vec2<u8>& target)
    {
        target_ = target;
    }


    Island* parent() const
    {
        return parent_;
    }


    Island* destination() const
    {
        return destination_;
    }


    virtual const char* name() const = 0;


protected:
    enum State : u8 {
        launch,
        ready,
    };

    u8 state_ = State::launch;

    Microseconds timer_ = 0;
    Microseconds duration_ = 0;

    void update_sprite(App& app);

private:
    Island* parent_;
    Island* destination_;
    Vec2<u8> grid_pos_;
    Vec2<s16> anchor_;

protected:
    std::optional<Vec2<u8>> target_;
};



} // namespace skyland
