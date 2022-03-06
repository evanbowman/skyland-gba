#pragma once


#include "projectile.hpp"



namespace skyland {



class Island;



class ArcBolt : public Projectile
{
public:
    ArcBolt(const Vec2<Float>& position,
            const Vec2<Float>& target,
            Island* source,
            const Vec2<u8>& origin_tile);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


    void set_step_vector(const Vec2<Float>& val)
    {
        step_vector_ = val;
    }


    void set_timer(Microseconds value)
    {
        timer_ = value;
    }


private:

    void destroy(Platform& pfrm, App& app, bool explosion);

    Microseconds timer_ = 0;
    Microseconds anim_timer_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    Vec2<u8> origin_tile_;
};



} // namespace skyland
