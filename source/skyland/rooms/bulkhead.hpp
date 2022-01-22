#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"



namespace skyland {



class Bulkhead : public Room {
public:
    Bulkhead(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(App& app, u8 buffer[16][16]) override;
    void render_exterior(App& app, u8 buffer[16][16]) override;


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        if (open_) {
            Room::plot_walkable_zones(app, matrix);
        }
        // Else, do nothing, i.e. impassible.
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app);


    static void format_description(StringBuffer<512>& buffer);


    static Float ai_base_weight()
    {
        return 20.f;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "bulkhead-door";
    }


    static Icon icon()
    {
        return 648;
    }


    static Icon unsel_icon()
    {
        return 632;
    }


    void set_open(Platform& pfrm, App& app, bool open);


    bool is_open() const
    {
        return open_;
    }


private:
    bool open_ = true;
    bool interior_visible_ = false;
};



} // namespace skyland
