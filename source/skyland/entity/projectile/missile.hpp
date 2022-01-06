#pragma once


#include "projectile.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



class Island;



class Missile : public Projectile {
public:
    Missile(const Vec2<Float>& position,
            const Vec2<Float>& target,
            Island* source);


    void update(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void on_collision(Platform& pfrm, App& app, Entity& entity) override;


private:
    Microseconds timer_ = 0;
    Float speed_;
    Float target_x_;

    Island* source_;

    enum class State {
        rising,
        wait,
        falling,
    } state_ = State::rising;
};



extern SharedVariable missile_damage;



} // namespace skyland
