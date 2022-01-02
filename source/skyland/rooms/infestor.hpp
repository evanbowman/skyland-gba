#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Infestor : public Room {
public:
    Infestor(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


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
        return {1, 1};
    }


    static const char* name()
    {
        return "infestor";
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    //    ScenePtr<Scene> select(Platform& pfrm, App&) override;


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


    static Icon icon()
    {
        return 552;
    }


    static Icon unsel_icon()
    {
        return 536;
    }


private:
    static constexpr const Microseconds reload_time = milliseconds(3500);


    Microseconds reload_ = reload_time;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
