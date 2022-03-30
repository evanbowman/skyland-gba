#pragma once

#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Lava : public Room
{
public:
    Lava(Island* parent, const Vec2<u8>& position, const char* name = name());


    void set_flood_parent(Vec2<u8> parent)
    {
        flood_parent_ = parent;
    }


    void render_scaffolding(App& app, u8 buffer[16][16]) override
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    static Category category()
    {
        return Category::misc;
    }


    static u32 properties()
    {
        return RoomProperties::disallow_chimney |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible | RoomProperties::roof_hidden |
               RoomProperties::fluid | RoomProperties::fragile |
               RoomProperties::destroy_quietly;
    }


    static Float ai_base_weight()
    {
        return 1.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "lava";
    }


    static SystemString ui_name()
    {
        return SystemString::block_lava;
    }


    bool description_visible() override
    {
        return true;
    }


    static Icon icon()
    {
        return 2120;
    }


    static Icon unsel_icon()
    {
        return 2136;
    }


    void refresh()
    {
        decay_ = 0;
    }


    virtual void
    check_flood_parent(Platform& pfrm, App& app, Microseconds delta);


protected:
    Microseconds decay_ = 0;

    Microseconds damage_timer_ = 0;

    Vec2<u8> flood_parent_;
    bool has_flood_parent_ = true;

    Microseconds flood_timer_ = 0;
};



class LavaSource : public Lava
{
public:
    LavaSource(Island* parent, const Vec2<u8>& position);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void
    check_flood_parent(Platform& pfrm, App& app, Microseconds delta) override;



    static const char* name()
    {
        return "lava-source";
    }


    static u32 properties()
    {
        return Lava::properties() & ~RoomProperties::not_constructible;
    }
};



} // namespace skyland
