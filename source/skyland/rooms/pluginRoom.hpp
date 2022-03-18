#pragma once

#include "skyland/room.hpp"



namespace skyland
{


class PluginRoom : public Room
{
public:
    PluginRoom(Island* parent, const Vec2<u8>& position, RoomMeta* metaclass);


    void update(Platform&, App&, Microseconds delta) override;


    void rewind(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]);


    void render_exterior(App& app, u8 buffer[16][16]);


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    void set_target(Platform& pfrm, App& app, const Vec2<u8>& target) override;


    void unset_target(Platform& pfrm, App& app) override;


    ScenePtr<Scene>
    select(Platform& pfrm, App& app, const Vec2<u8>& cursor) override;


    bool description_visible() override
    {
        return true;
    }


    static Icon icon()
    {
        return 1512;
    }


    static Icon unsel_icon()
    {
        return 1528;
    }


private:
    Microseconds timer_ = 0;
    std::optional<Vec2<u8>> target_;
};



} // namespace skyland
