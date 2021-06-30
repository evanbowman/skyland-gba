#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class IonCannon : public Room {
public:
    IonCannon(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    bool has_roof() override
    {
        return false;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "ion-cannon";
    }


    static Coins cost()
    {
        return 800;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    void set_target(const Vec2<u8>& target) override
    {
        target_ = target;
    }


    void unset_target() override
    {
        target_.reset();
    }


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Power consumes_power()
    {
        return 30;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


private:
    static constexpr const Microseconds reload_time = milliseconds(3500);


    Microseconds reload_ = reload_time;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
