#pragma once


#include "skyland/entity.hpp"



namespace skyland {



class SmallBird : public Entity {
public:
    SmallBird(const Vec2<Float>& position, Float speed)
        : Entity({{}, {}}), timer1_(0), timer2_(0), speed_(speed)
    {
        sprite_.set_position(position);
        sprite_.set_texture_index(28);
        sprite_.set_size(Sprite::Size::w16_h32);
        sprite_.set_origin({8, 8});
        sprite_.set_priority(3);
    }


    void update(Platform&, App&, Microseconds delta) override
    {
        timer1_ += delta;
        timer2_ += delta;

        if (timer1_ > milliseconds(90)) {
            timer1_ = 0;

            auto kf = sprite_.get_texture_index();
            if (kf > 28 + 6) {
                kf = 28;
            } else {
                ++kf;
            }
            sprite_.set_texture_index(kf);
        }

        auto pos = sprite_.get_position();
        pos.x += speed_ * delta;
        sprite_.set_position(pos);

        if (timer2_ > seconds(10)) {
            kill();
        }
    }


    auto age() const
    {
        return timer2_;
    }


    Float speed() const
    {
        return speed_;
    }


private:
    Microseconds timer1_;
    Microseconds timer2_;
    Float speed_;
};



} // namespace skyland
