#pragma once


#include "skyland/entity.hpp"



namespace skyland {



class Cannonball : public Entity {
public:
    Cannonball(const Vec2<Float>& position) :
        Entity({{16, 16}, {8, 8}})
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(12);
    }


    void update(Platform&, App&, Microseconds delta) override
    {
        auto pos = sprite_.get_position();
        pos.x += 2;
        sprite_.set_position(pos);

        timer_ += delta;

        if (timer_ > seconds(1)) {
            kill();
        }
    }

private:
    Microseconds timer_ = 0;

};



}
