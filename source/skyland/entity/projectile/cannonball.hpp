#pragma once


#include "projectile.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class Cannonball : public Projectile {
public:
    Cannonball(const Vec2<Float>& position,
               const Vec2<Float>& target,
               Island* source,
               const Vec2<u8>& origin_tile);


    void update(Platform&, App&, Microseconds delta) override;


    void on_collision(Platform& pfrm, App& app, Room&) override;


private:
    Microseconds timer_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    Vec2<u8> origin_tile_;
};



} // namespace skyland
