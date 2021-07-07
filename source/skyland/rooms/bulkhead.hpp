#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Bulkhead : public Room {
public:
    Bulkhead(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        if (open_) {
            Room::plot_walkable_zones(matrix);
        }
        // Else, do nothing, i.e. impassible.
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app);


    static Float ai_base_weight()
    {
        return 20.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "bulkhead";
    }


    static Coins cost()
    {
        return 500;
    }


    static Power consumes_power()
    {
        return 10;
    }


    void set_open(Platform& pfrm, bool open);


private:
    bool open_ = true;
    bool interior_visible_ = false;
};



} // namespace skyland
