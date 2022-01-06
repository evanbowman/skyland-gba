#pragma once



#include "skyland/room.hpp"
#include "skyland/sharedVariable.hpp"



namespace skyland {



extern SharedVariable decimator_reload_ms;



class Decimator : public Room {
public:
    Decimator(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta);


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override;


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "decimator";
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    static Float ai_base_weight()
    {
        return 1500.f;
    }


    bool has_roof() override
    {
        return false;
    }


    bool description_visible() override
    {
        return true;
    }


    static Conditions::Value conditions()
    {
        return Conditions::foundry_required;
    }


    Microseconds reload_time_remaining() const override
    {
        return reload_;
    }


private:

    int counter_ = 0;


    Microseconds reload_ = 1000 * decimator_reload_ms;
};



} // namespace skyland
