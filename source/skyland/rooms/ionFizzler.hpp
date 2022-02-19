#pragma once


#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



class IonFizzler : public Room {
public:
    IonFizzler(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static void format_description(StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::wall;
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


    static SystemString ui_name()
    {
        return SystemString::block_ion_fizzler;
    }


    static Icon icon()
    {
        return 1240;
    }


    static Icon unsel_icon()
    {
        return 1256;
    }


    static u32 properties()
    {
        return RoomProperties::manufactory_required |
               RoomProperties::disallow_chimney | RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage |
               RoomProperties::cancels_ion_damage;
    }
};



} // namespace skyland
