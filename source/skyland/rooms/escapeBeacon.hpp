#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class EscapeBeacon : public Room
{
public:
    EscapeBeacon(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static const char* name()
    {
        return "escape-beacon";
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::misc;
    }


    bool description_visible() override
    {
        return true;
    }


    static Float ai_base_weight()
    {
        return 1000.f;
    }


    static Vec2<u8> size()
    {
        return {1, 3};
    }


    static SystemString ui_name()
    {
        return SystemString::block_escape_beacon;
    }


    static Icon icon()
    {
        return 1208;
    }


    static Icon unsel_icon()
    {
        return 1224;
    }


    static u32 properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden |
               RoomProperties::accepts_ion_damage |
               RoomProperties::disabled_in_tutorials;
    }


    ScenePtr<Scene> select(Platform&, App&, const Vec2<u8>& cursor) override;


    Microseconds reload_time_remaining() const override
    {
        if (not activated_) {
            return 0;
        } else {
            return timer_;
        }
    }


private:
    bool activated_ = false;
    Microseconds timer_;
};



} // namespace skyland
