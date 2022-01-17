#pragma once

#include "skyland/room.hpp"



namespace skyland {



class CargoBay : public Room {
public:
    CargoBay(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;

    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    static Float ai_base_weight()
    {
        return 800.f;
    }


    static Vec2<u8> size()
    {
        return {2, 2};
    }


    static const char* name()
    {
        return "cargo-bay";
    }


    static Icon icon()
    {
        return 776;
    }


    static Icon unsel_icon()
    {
        return 760;
    }


    const char* cargo()
    {
        return cargo_;
    }


    bool set_cargo(const char* cargo);


private:
    char cargo_[20];
};



} // namespace skyland
