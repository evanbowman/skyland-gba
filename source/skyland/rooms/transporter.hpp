#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Transporter : public Room {
public:
    Transporter(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(u8 buffer[16][16]) override;
    void render_exterior(u8 buffer[16][16]) override;


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    void random_transport_occupant(Platform& pfrm, App& app);


    void recover_character(Platform&, App& app, const Vec2<u8>& pos);


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


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    static Icon icon()
    {
        return 904;
    }


    static Icon unsel_icon()
    {
        return 888;
    }


    static const Microseconds recharge_time = seconds(18);


    bool ready() const;


    Microseconds reload_time_remaining() const override
    {
        return recharge_;
    }


private:
    Microseconds recharge_ = recharge_time;
};



} // namespace skyland
