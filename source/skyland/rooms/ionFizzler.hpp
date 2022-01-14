#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class IonFizzler : public Room {
public:
    IonFizzler(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Category category()
    {
        return Category::wall;
    }


    // Plenty of rooms, like missile silos, and forcefields, look super awkward
    // if the game spawns chimneys over top of them.
    bool disallow_chimney() override
    {
        return true;
    }


    bool has_roof() override
    {
        return false;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 10.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "ion-fizzler";
    }


    static Icon icon()
    {
        return 1240;
    }


    static Icon unsel_icon()
    {
        return 1256;
    }


    static Conditions::Value conditions()
    {
        return Conditions::foundry_required;
    }
};



} // namespace skyland
