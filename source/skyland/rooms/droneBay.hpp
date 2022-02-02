#pragma once


#include "skyland/coins.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



extern SharedVariable drone_bay_reload_ms;



class DroneBay : public Room {
public:
    DroneBay(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;
    void rewind(Platform&, App&, Microseconds delta) override;


    void ___rewind___finished_reload(Platform&, App&) override;

    void ___rewind___ability_used(Platform&, App&) override;



    void display(Platform::Screen& screen) override;


    void finalize(Platform&, App&) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;



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


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
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


    static u32 properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden;
    }


    std::optional<SharedEntityRef<Drone>> drone() const
    {
        return drone_;
    }


    void attach_drone(Platform& pfrm, App& app, SharedEntityRef<Drone> drone);


    void detach_drone(Platform& pfrm, App& app, bool quiet = false);


    void start_reload()
    {
        reload_ = 1000 * drone_bay_reload_ms;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


private:
    std::optional<SharedEntityRef<Drone>> drone_;


    Microseconds reload_ = 0;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
