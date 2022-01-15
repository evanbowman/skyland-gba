#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



extern SharedVariable missile_silo_reload_ms;



class MissileSilo : public Room {
public:
    MissileSilo(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


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


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    Microseconds reload_time_remaining() const override
    {
        return load_;
    }


private:
    Microseconds load_ = 1000 * missile_silo_reload_ms;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
