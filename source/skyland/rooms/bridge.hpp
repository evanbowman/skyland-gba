#pragma once


#include "decoration.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/systemString.hpp"



namespace skyland {



void show_island_interior(Platform& pfrm, App& app, Island* island);



class Bridge : public Decoration {
public:
    Bridge(Island* parent, const Vec2<u8>& position)
        : Decoration(parent, name(), position)
    {
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        matrix[position().x][position().y] = true;
        matrix[position().x + 1][position().y] = true;
    }


    void render_interior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = InteriorTile::bridge;
        buffer[position().x + 1][position().y] = InteriorTile::bridge;
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::bridge;
        buffer[position().x + 1][position().y] = Tile::bridge;
    }


    void display(Platform::Screen& screen)
    {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y < 700) {
                screen.draw(c->sprite());
            }
        }
    }


    static u32 properties()
    {
        return Decoration::properties() & ~RoomProperties::roof_hidden;
    }


    ScenePtr<Scene> select(Platform& pfrm, App& app)
    {
        // Unlike most rooms, the bridge shows inhabitants while viewing a
        // castle's exterior. If selecting a character, we want to show the
        // interior representation of the castle.
        if (not characters().empty() and not parent()->interior_visible()) {

            show_island_interior(pfrm, app, parent());
        }

        return Room::do_select(pfrm, app);
    }


    static const char* name()
    {
        return "bridge";
    }


    static SystemString ui_name()
    {
        return SystemString::block_bridge;
    }


    static Vec2<u8> size()
    {
        return {2, 1};
    }


    static Icon icon()
    {
        return 1544;
    }


    static Icon unsel_icon()
    {
        return 1560;
    }
};



} // namespace skyland
