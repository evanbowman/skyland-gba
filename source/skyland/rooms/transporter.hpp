#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Transporter : public Room {
public:
    Transporter(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    void random_transport_occupant(Platform& pfrm, App& app);


    void recover_character(App& app, const Vec2<u8>& pos);


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "transporter";
    }


    static Float ai_base_weight()
    {
        return 900.f;
    }


    static Coins cost()
    {
        return 999;
    }


    static Power consumes_power()
    {
        return 40;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    static const Microseconds recharge_time = seconds(20);


    bool ready() const
    {
        return recharge_ == 0;
    }


private:
    Microseconds recharge_ = recharge_time;
};



} // namespace skyland
