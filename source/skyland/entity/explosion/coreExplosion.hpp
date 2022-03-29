#pragma once

#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class CoreExplosionQuarter : public Entity
{
public:
    CoreExplosionQuarter(Platform::DynamicTexturePtr dt,
                         const Vec2<Float>& pos,
                         int quarter)
        : Entity({{}, {}}), dt_(dt), timer_(0), quarter_(quarter)
    {
        sprite_.set_texture_index(dt->mapping_index());

        auto p = pos;

        switch (quarter) {
        case 1:
            sprite_.set_flip({true, false});
            p.x += 32;
            break;

        case 2:
            sprite_.set_flip({false, true});
            p.y += 32;
            break;

        case 3:
            sprite_.set_flip({true, true});
            p.x += 32;
            p.y += 32;
            break;
        }

        sprite_.set_position(p);

        dt_->remap(70 * 2);
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);


            if (keyframe_ == 5) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_++;
            }
        }
    }


    void rewind(Platform& pfrm, App& app, Microseconds delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ += milliseconds(200);

            if (keyframe_ == 0) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_--;
            }
        }
    }


private:
    Platform::DynamicTexturePtr dt_;
    Microseconds timer_;
    int quarter_;
    int keyframe_ = 0;
};



inline void core_explosion(Platform& pfrm, App& app, const Vec2<Float>& pos)
{
    auto dt = pfrm.make_dynamic_texture();
    if (dt) {
        auto p = pos;
        p.x -= 32;
        p.y -= 32;
        auto make_segment = [&](int q) {
            return app.effects().push(
                app.alloc_entity<CoreExplosionQuarter>(pfrm, *dt, p, q));
        };
        make_segment(3);
        make_segment(2);
        make_segment(1);
        make_segment(0);
    }
}



} // namespace skyland
