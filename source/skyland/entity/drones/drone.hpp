#pragma once

#include "skyland/entity.hpp"
#include "memory/rc.hpp"



namespace skyland {



class Island;



class Drone : public Entity, public IntrusiveRcControlBlock<Drone> {
public:

    Drone(Island* parent,
          const Vec2<u8>& grid_pos) :
        Entity({{10, 10}, {8, 8}}),
        parent_(parent),
        grid_pos_({grid_pos.x, u8(grid_pos.y)})
    {
        sprite_.set_texture_index(64);
        sprite_.set_size(Sprite::Size::w16_h32);

        health_ = 40;
    }


    void update(Platform&, App&, Microseconds delta) override;


    const Vec2<u8>& position() const
    {
        return grid_pos_;
    }


    void set_movement_path(const Vec2<u8>& position)
    {
        grid_pos_ = position;
    }


    void set_target(const Vec2<u8>& target)
    {
        target_ = target;
    }


private:
    Island* parent_;
    Vec2<u8> grid_pos_;
    Vec2<u8> target_;
};



}
