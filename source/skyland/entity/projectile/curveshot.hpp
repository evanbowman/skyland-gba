#pragma once


#include "projectile.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



class Curveshot : public Projectile
{
public:
    Curveshot(const Vec2<Float>& position,
              const Vec2<Float>& target,
              Island* source,
              Island* dest,
              const Vec2<u8>& origin_tile,
              const Vec2<u8>& dest_tile);


    void set_step_vector(const Vec2<Float>& val)
    {
        step_vector_ = val;
    }


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void on_collision(Platform& pfrm, App& app, Entity&) override;


private:
    void destroy(Platform& pfrm, App& app, bool explosion);

    Float y_base_ = 0;
    Microseconds timer_ = 0;
    Microseconds time_to_target_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    Vec2<u8> origin_tile_;
    u8 height_;
};



} // namespace skyland
