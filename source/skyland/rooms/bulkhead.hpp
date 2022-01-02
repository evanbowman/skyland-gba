#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Bulkhead : public Room {
public:
    Bulkhead(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(u8 buffer[16][16]) override;
    void render_exterior(u8 buffer[16][16]) override;


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


    static Icon icon()
    {
        return 648;
    }


    static Icon unsel_icon()
    {
        return 632;
    }


    void set_open(Platform& pfrm, bool open);


    bool is_open() const
    {
        return open_;
    }


    Microseconds reload_time_remaining() const override
    {
        return cooldown_;
    }


private:
    bool open_ = true;
    bool interior_visible_ = false;
    Microseconds cooldown_ = 0;
};



} // namespace skyland
