#pragma once


#include "skyland/entity.hpp"



namespace skyland {



class SmokePuff : public Entity {
public:
    SmokePuff(const Vec2<Float>& position) : Entity({{}, {}})
    {
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_texture_index(27);
        sprite_.set_position(position);
        sprite_.set_alpha(Sprite::Alpha::translucent);
        sprite_.set_origin({8, 8});
    }


    void update(Platform&, App&, Microseconds delta)
    {
        timer_ += delta;

        auto pos = sprite_.get_position();

        pos.x -= + Float(delta) * 0.00001f;
        pos.y -= + Float(delta) * 0.00001f;

        const s16 shrink_amount = interpolate(-450, -24, Float(timer_) / seconds(3));

        sprite_.set_scale({shrink_amount, shrink_amount});

        sprite_.set_position(pos);

        if (timer_ > seconds(3)) {
            this->kill();
        }
    }


private:
    Microseconds timer_ = 0;
};



}
