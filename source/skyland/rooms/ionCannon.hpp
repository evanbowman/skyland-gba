#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



extern SharedVariable ion_cannon_reload_ms;



class IonCannon : public Room {
public:
    IonCannon(Island* parent, const Vec2<u8>& position);


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


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "ion-cannon";
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


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static Conditions::Value conditions()
    {
        return Conditions::workshop_required;
    }


    static Icon icon()
    {
        return 840;
    }


    static Icon unsel_icon()
    {
        return 824;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


private:
    Microseconds reload_ = 1000 * ion_cannon_reload_ms;

    std::optional<Vec2<u8>> target_;
};


} // namespace skyland
