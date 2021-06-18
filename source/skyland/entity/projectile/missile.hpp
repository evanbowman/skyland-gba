#pragma once


#include "projectile.hpp"



namespace skyland {



class Island;



class Missile : public Projectile {
public:

    Missile(const Vec2<Float>& position,
            const Vec2<Float>& target);


    void update(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    static const Health deals_damage = 100;


private:
    Microseconds timer_ = 0;
    Float speed_;
    Float target_x_;

    enum class State {
        rising,
        wait,
        falling,
    } state_ = State::rising;
};



}
