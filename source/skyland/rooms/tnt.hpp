#pragma once



#include "skyland/room.hpp"



namespace skyland {



class Explosive : public Room {
public:
    Explosive(Island* parent,
              const Vec2<u8>& position,
              const char* class_name = name());


    void finalize(Platform& pfrm, App& app) override;


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    bool description_visible() override
    {
        return true;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    ScenePtr<Scene> select(Platform& pfrm, App&) override;


    static Category category()
    {
        return Category::misc;
    }


    static u32 properties()
    {
        return RoomProperties::roof_hidden | RoomProperties::flag_mount |
               RoomProperties::disabled_in_tutorials;
    }


    static Float ai_base_weight()
    {
        return 2000.f;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static const char* name()
    {
        return "dynamite";
    }


    static Icon icon()
    {
        return 1672;
    }


    static Icon unsel_icon()
    {
        return 1688;
    }


    void apply_damage(Platform& pfrm, App& app, Health damage) override;


    void ignite(Platform& pfrm, App& app, int range, Health damage);


protected:
    bool ignition_ = false;

private:
    Microseconds damage_timer_ = 0;
};



class TNT : public Explosive {
public:
    TNT(Island* parent, const Vec2<u8>& position)
        : Explosive(parent, position, name())
    {
    }


    void finalize(Platform& pfrm, App& app) override
    {
        Room::finalize(pfrm, app);

        if (not ignition_) {
            return;
        } else {
            ignite(pfrm, app, 2, 200);
        }
    }


    static u32 properties()
    {
        return Explosive::properties() | RoomProperties::manufactory_required;
    }


    static const char* name()
    {
        return "dynamite-ii";
    }


    static Icon icon()
    {
        return 1704;
    }


    static Icon unsel_icon()
    {
        return 1720;
    }


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;
};



} // namespace skyland
