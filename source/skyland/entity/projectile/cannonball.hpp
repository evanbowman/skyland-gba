#pragma once


#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class Cannonball : public Entity {
public:
    Cannonball(const Vec2<Float>& position, const Vec2<Float>& target) :
        Entity({{10, 10}, {8, 8}})
    {
        sprite_.set_position(position);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(18);

        sprite_.set_origin({8, 8});

        static const Float speed = 0.00015f;
        step_vector_ = direction(position, target) * speed;
    }


    void update(Platform&, App&, Microseconds delta) override
    {
        auto pos = sprite_.get_position();
        pos = pos + Float(delta) * step_vector_;
        sprite_.set_position(pos);

        timer_ += delta;

        if (timer_ > seconds(1)) {
            kill();
        }
    }


    void on_collision(Platform&, App& app, Room&) override
    {
        kill();
        app.camera().shake(8);
    }


private:
    Microseconds timer_ = 0;
    Vec2<Float> step_vector_;
};



}
