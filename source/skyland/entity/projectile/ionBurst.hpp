#pragma once


#include "projectile.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class IonBurst : public Projectile {
public:
    IonBurst(const Vec2<Float>& position,
             const Vec2<Float>& target,
             Island* source);


    void update(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


private:
    Microseconds timer_ = 0;
    Microseconds anim_timer_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;
};



} // namespace skyland
