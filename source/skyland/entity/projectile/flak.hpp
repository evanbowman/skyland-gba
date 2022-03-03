#pragma once


#include "projectile.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class Flak : public Projectile
{
public:
    Flak(const Vec2<Float>& position,
         const Vec2<Float>& target,
         Island* source,
         const Vec2<u8>& origin_tile);


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


    static void burst(Platform& pfrm,
                      App& app,
                      const Vec2<Float>& position,
                      Room& origin_room);


private:
    Microseconds timer_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    Microseconds flicker_time_ = 0;

    bool destroyed_ = false;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    Vec2<u8> origin_tile_;
};



extern SharedVariable flak_r1_damage;
extern SharedVariable flak_r2_damage;
extern SharedVariable flak_r3_damage;



} // namespace skyland
