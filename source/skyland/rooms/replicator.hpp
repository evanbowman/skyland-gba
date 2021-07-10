#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Replicator : public Room {
public:
    Replicator(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    static Vec2<u8> size()
    {
        return {2, 4};
    }


    static const char* name()
    {
        return "replicator";
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static Coins cost()
    {
        return 2500;
    }


    static Power consumes_power()
    {
        return 80;
    }


    static Icon icon()
    {
        return 808;
    }


    static Icon unsel_icon()
    {
        return 792;
    }


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    bool create_replicant(Platform& pfrm);


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


private:

    static const auto recharge_time = seconds(5);

    Microseconds recharge_timer_ = 0;
};



} // namespace skyland
