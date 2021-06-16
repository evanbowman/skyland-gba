#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class MissileSilo : public Room {
public:
    MissileSilo(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    bool has_roof() override
    {
        return false;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "missile silo";
    }


    static Coins cost()
    {
        return 1800;
    }


    static Float ai_base_weight()
    {
        return 600.f;
    }


    void set_target(const Vec2<u8>& target) override
    {
        target_ = target;
    }


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


private:
    static constexpr const Microseconds reload_time = seconds(5);


    Microseconds reload_ = reload_time;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
