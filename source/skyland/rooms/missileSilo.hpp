#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class MissileSilo : public Room {
public:
    MissileSilo(Island* parent, const Vec2<u8>& position);


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


    virtual bool disallow_chimney()
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "missile-silo";
    }


    static Float ai_base_weight()
    {
        return 600.f;
    }


    static Icon icon()
    {
        return 584;
    }


    static Icon unsel_icon()
    {
        return 568;
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


    Microseconds reload_time_remaining() const override
    {
        return load_;
    }


private:
    static constexpr const Microseconds load_time = seconds(7);


    Microseconds load_ = load_time;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
