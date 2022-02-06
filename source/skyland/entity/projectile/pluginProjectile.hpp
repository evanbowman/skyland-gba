#pragma once


#include "projectile.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



class PluginProjectile : public Projectile {
public:
    PluginProjectile(const Vec2<Float>& position,
                     const Vec2<Float>& target,
                     Island* source,
                     const Vec2<u8>& origin_tile,
                     u16 graphics_tile,
                     Health damage,
                     bool flip);


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
    void timestream_record_destroyed(Platform& pfrm, App& app);

    Microseconds timer_ = 0;
    Vec2<Float> step_vector_;
    Island* source_;

    // We need to keep track of the origin tile coords, to prevent cannons from
    // shooting themselves.
    Vec2<u8> origin_tile_;

    Health damage_;
};



} // namespace skyland
