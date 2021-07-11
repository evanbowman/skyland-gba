#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class FlakGun : public Room {
public:
    FlakGun(Island* parent, const Vec2<u8>& position);


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
        return {2, 1};
    }


    static const char* name()
    {
        return "flak-gun";
    }


    static Coins cost()
    {
        return 1900;
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
        // characters cannot walk through a flak gun.
    }


    static Power consumes_power()
    {
        return 34;
    }


private:
    static constexpr const Microseconds reload_time = milliseconds(4500);

    Microseconds reload_ = reload_time;


    std::optional<Vec2<u8>> target_;
};



} // namespace skyland
