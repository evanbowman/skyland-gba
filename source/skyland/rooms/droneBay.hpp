#pragma once


#include "skyland/coins.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/room.hpp"



namespace skyland {



class DroneBay : public Room {
public:
    DroneBay(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;

    void display(Platform::Screen& screen) override;



    void render_interior(u8 buffer[16][16]) override;
    void render_exterior(u8 buffer[16][16]) override;


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
        return "drone-bay";
    }


    static Float ai_base_weight()
    {
        return 1200.f;
    }


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Icon icon()
    {
        return 1032;
    }


    static Icon unsel_icon()
    {
        return 1016;
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    std::optional<SharedEntityRef<Drone>> drone() const
    {
        return drone_;
    }


    void attach_drone(SharedEntityRef<Drone> drone)
    {
        drone_ = drone;
    }


    void disconnect_drone()
    {
        drone_.reset();
    }


    void start_reload()
    {
        reload_ = reload_time;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


private:
    static constexpr const Microseconds reload_time = seconds(20);


    std::optional<SharedEntityRef<Drone>> drone_;


    Microseconds reload_ = 0;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
