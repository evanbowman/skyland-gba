#pragma once


#include "projectile.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland
{



class Island;



class Missile : public Projectile
{
public:
    Missile(const Vec2<Float>& position,
            const Vec2<Float>& target,
            u8 source_x,
            u8 source_y,
            Island* source);


    enum class State : u8 {
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


protected:
    virtual void destroy(Platform& pfrm, App& app);

private:
    Microseconds timer_ = 0;
    Float target_x_;

    Island* source_;
    u8 source_x_;
    u8 source_y_;

    State state_ = State::rising;
};



extern SharedVariable missile_damage;



class RocketBomb : public Missile
{
public:

    RocketBomb(const Vec2<Float>& position,
               const Vec2<Float>& target,
               u8 source_x,
               u8 source_y,
               Island* source) :
        Missile(position, target, source_x, source_y, source)
    {
        sprite_.set_texture_index(88);
    }



    void on_collision(Platform& pfrm, App& app, Room&) override;


protected:

    void destroy(Platform& pfrm, App& app) override;

};



} // namespace skyland
