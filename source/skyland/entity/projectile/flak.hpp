#pragma once


#include "projectile.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class Flak : public Projectile {
public:
    Flak(const Vec2<Float>& position,
         const Vec2<Float>& target,
         Island* source,
         const Vec2<u8>& origin_tile);


    void update(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;



    static const Health r1_damage = 16;
    static const Health r2_damage = 12;
    static const Health r3_damage = 10;

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



} // namespace skyland
