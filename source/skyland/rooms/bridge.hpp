#pragma once


#include "decoration.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void show_island_interior(Platform& pfrm, App& app, Island* island);



class Bridge : public Decoration
{
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
        if (buffer[position().x][position().y + 1] == 0) {
            buffer[position().x][position().y + 1] = Tile::bridge_truss;
        }
        if (buffer[position().x + 1][position().y + 1] == 0) {
            buffer[position().x + 1][position().y + 1] = Tile::bridge_truss;
        }
    }


    void render_exterior(App& app, u8 buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::bridge;
        buffer[position().x + 1][position().y] = Tile::bridge;
        if (buffer[position().x][position().y + 1] == 0) {
            buffer[position().x][position().y + 1] = Tile::bridge_truss;
        }
        if (buffer[position().x + 1][position().y + 1] == 0) {
            buffer[position().x + 1][position().y + 1] = Tile::bridge_truss;
        }
    }


    void display(Platform::Screen& screen) override
    {
        for (auto& c : characters()) {
            const auto& pos = c->sprite().get_position();
            if (pos.y < 700) {
                auto spr = c->sprite();
                spr.set_priority(3);
                screen.draw(spr);
            }
        }
    }


    void display_on_hover(Platform::Screen& screen,
                          App& app,
                          const Vec2<u8>& cursor) override
    {
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
