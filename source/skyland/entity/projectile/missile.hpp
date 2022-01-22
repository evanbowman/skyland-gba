#pragma once


#include "projectile.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



class Island;



class Missile : public Projectile {
public:
    Missile(const Vec2<Float>& position,
            const Vec2<Float>& target,
            u8 source_x,
            Island* source);


    enum class State {
        rising,
        wait,
        falling,
    };


    void set_state(State state)
    {
        state_ = state;
    }


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void on_collision(Platform& pfrm, App& app, Entity& entity) override;


private:
    Microseconds timer_ = 0;
    Float target_x_;

    Island* source_;
    u8 source_x_;

    State state_ = State::rising;
};



extern SharedVariable missile_damage;



} // namespace skyland
