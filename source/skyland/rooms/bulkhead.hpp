#pragma once

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/island.hpp" // move to .cpp



namespace skyland {



class Bulkhead : public Room {
public:
    Bulkhead(Island* parent, const Vec2<u8>& position);


    void update(Platform&, App&, Microseconds delta) override;


    void render_interior(Platform& pfrm, Layer layer) override;
    void render_exterior(Platform& pfrm, Layer layer) override;


    void plot_walkable_zones(bool matrix[16][16]) override
    {
        if (open_) {
            Room::plot_walkable_zones(matrix);
        }
        // Else, do nothing, i.e. impassible.
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app)
    {
        if (not open_) {
            open_ = true;
        } else {
            // We don't want to close the door on any characters, so make sure
            // that we don't have any occupants.
            if (length(characters()) == 0) {
                open_ = false;

                parent()->on_layout_changed({
                        position().x,
                        u8(position().y + 1)
                    });
            }
        }
        return null_scene();
    }


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
        return "bulkhead";
    }


    static Coins cost()
    {
        return 500;
    }


    static Power consumes_power()
    {
        return 10;
    }


private:
    bool open_ = true;
};



} // namespace skyland
