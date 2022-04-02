#include "mycelium.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Mycelium::Mycelium(Island* parent, const Vec2<u8>& position, const char* n)
    : Room(parent, n, position)
{
}



void Mycelium::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (parent()->is_destroyed()) {
        return;
    }

    Room::ready();

    flood_timer_ += delta;

    if (flood_timer_ > flood_time) {
        flood_timer_ -= flood_time;

        const auto x = position().x;
        const auto y = position().y;


        auto substrate = [&](u8 x, u8 y) {
            if (auto room = parent()->get_room({x, y})) {
                // Mycelium substrate must be non-mycelium room.
                return room->metaclass() not_eq metaclass() and
                       not(room->properties() & RoomProperties::fluid) and
                       not str_eq(room->name(), "forcefield");
            }

            return false;
        };

        if (not substrate(x + 1, y) and not substrate(x - 1, y) and
            not substrate(x, y - 1) and not substrate(x, y + 1) and
            not substrate(x + 1, y + 1) and not substrate(x - 1, y - 1) and
            not substrate(x + 1, y - 1) and not substrate(x - 1, y + 1) and
            y not_eq 14) {
            this->apply_damage(pfrm, app, health_upper_limit());
            return;
        }

        auto slot_valid = [&](u8 x, u8 y) {
            if (x > parent()->terrain().size() - 1 or y > 14 or
                y < construction_zone_min_y) {
                return false;
            }
            if (not parent()->get_room({x, y})) {
                if (y == 14) {
                    return true;
                }


                return substrate(x + 1, y) or substrate(x - 1, y) or
                       substrate(x, y - 1) or substrate(x, y + 1) or
                       substrate(x + 1, y + 1) or substrate(x - 1, y - 1) or
                       substrate(x + 1, y - 1) or substrate(x - 1, y + 1);
            }
            return false;
        };

        auto spread = [&](u8 x, u8 y) {
            (*metaclass())->create(pfrm, app, parent(), {x, y}, false);

            parent()->schedule_repaint();

            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomCreated p;
                p.x_ = x;
                p.y_ = y;
                app.time_stream().push(app.level_timer(), p);
            } else {
                time_stream::event::OpponentRoomCreated p;
                p.x_ = x;
                p.y_ = y;
                app.time_stream().push(app.level_timer(), p);
            }
        };

        auto try_spread = [&](u8 x, u8 y) {
            if (slot_valid(x, y)) {
                spread(x, y);
            }
        };

        try_spread(x + 1, y);
        try_spread(x - 1, y);
        try_spread(x, y + 1);
        try_spread(x, y - 1);
        try_spread(x + 1, y + 1);
        try_spread(x - 1, y - 1);
        try_spread(x - 1, y + 1);
        try_spread(x + 1, y - 1);
    }
}



void Mycelium::render_interior(App& app, u8 buffer[16][16])
{
    bool above = false;
    bool below = false;

    if (auto room = parent()->get_room({position().x, u8(position().y - 1)})) {
        if (room->metaclass() == metaclass()) {
            above = true;
        }
    }

    if (auto room = parent()->get_room({position().x, u8(position().y + 1)})) {
        if (room->metaclass() == metaclass()) {
            below = true;
        }
    }

    if (above and below) {
        buffer[position().x][position().y] = InteriorTile::mycelium_middle;
    } else if (above) {
        buffer[position().x][position().y] = InteriorTile::mycelium_bottom;
    } else if (below) {
        buffer[position().x][position().y] = InteriorTile::mycelium_top;
    } else {
        buffer[position().x][position().y] = InteriorTile::mycelium;
    }
}



void Mycelium::render_exterior(App& app, u8 buffer[16][16])
{
    bool above = false;
    bool below = false;

    if (auto room = parent()->get_room({position().x, u8(position().y - 1)})) {
        if (room->metaclass() == metaclass()) {
            above = true;
        }
    }

    if (auto room = parent()->get_room({position().x, u8(position().y + 1)})) {
        if (room->metaclass() == metaclass()) {
            below = true;
        }
    }

    if (above and below) {
        buffer[position().x][position().y] = Tile::mycelium_middle;
    } else if (above) {
        buffer[position().x][position().y] = Tile::mycelium_bottom;
    } else if (below) {
        buffer[position().x][position().y] = Tile::mycelium_top;
    } else {
        buffer[position().x][position().y] = Tile::mycelium;
    }
}



} // namespace skyland
