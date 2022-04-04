#include "water.hpp"
#include "globals.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Water::Water(Island* parent, const Vec2<u8>& position, const char* name)
    : Room(parent, name, position)
{
}



void Water::check_flood_parent(Platform& pfrm, App& app, Microseconds delta)
{
    // NOTE: we want to destroy a water block if its parent water block no
    // longer exists.

    has_flood_parent_ = false;

    bool flood_source_is_water = false;

    if (auto room = parent()->get_room(flood_parent_)) {
        if (auto w = dynamic_cast<Water*>(room)) {
            flood_source_is_water = true;
            if (w->has_flood_parent_) {
                has_flood_parent_ = true;
            } else if (dynamic_cast<WaterSource*>(w)) {
                has_flood_parent_ = true;
            }
        }
    }

    if (not flood_source_is_water and not has_flood_parent_) {
        decay_ += delta;

        if (decay_ > milliseconds(300)) {
            __set_health(0);
        }
    } else {
        decay_ = 0;
    }
}



void Water::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (parent()->is_destroyed()) {
        return;
    }

    Room::ready();

    check_flood_parent(pfrm, app, delta);

    if (has_flood_parent_) {
        flood_timer_ += delta;
    }

    auto kill_fire = [&](u8 x, u8 y) {
                         if (parent()->fire_present({x, y})) {
                             parent()->fire_extinguish(pfrm, app, {x, y});
                         }
                     };

    auto x = position().x;
    auto y = position().y;

    if (x > 0) {
        kill_fire(x - 1, y);
    }

    if (y > 0) {
        kill_fire(x, y - 1);
    }

    if (x < 15) {
        kill_fire(x + 1, y);
    }

    if (y < 15) {
        kill_fire(x, y + 1);
    }


    if (flood_timer_ >= milliseconds(300)) {
        flood_timer_ -= milliseconds(300);

        auto flood = [&](u8 x, u8 y) {
            (*load_metaclass("water"))
                ->create(pfrm, app, parent(), {x, y}, false);

            parent()->schedule_repaint();

            if (auto room = parent()->get_room({x, y})) {
                if (auto w = dynamic_cast<Water*>(room)) {
                    w->set_flood_parent(position());
                }
            }

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

        auto floor = parent()->get_room({position().x, (u8)(position().y + 1)});
        if (not floor and position().y < 14) {
            flood(position().x, position().y + 1);
        } else if (not floor or
                   (floor and not((*floor->metaclass())->properties() &
                                  RoomProperties::fluid))) {
            // Floor is not a fluid, expand laterally.

            if (position().x < 15 and
                position().x < parent()->terrain().size() - 1 and
                not parent()->get_room(
                    {(u8)(position().x + 1), position().y})) {
                flood(position().x + 1, position().y);
            }

            if (position().x > 0 and
                not parent()->get_room(
                    {(u8)(position().x - 1), position().y})) {
                flood(position().x - 1, position().y);
            }
        }
    }
}



void Water::render_interior(App& app, u8 buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and (*above->metaclass())->properties() & RoomProperties::fluid) {
        buffer[position().x][position().y] = InteriorTile::water_column;
    } else {
        u8 x = position().x;
        auto left = parent()->get_room({(u8)(x - 1), position().y});
        auto right = parent()->get_room({(u8)(x + 1), position().y});

        if (left and not right) {
            buffer[position().x][position().y] = InteriorTile::water_right;
        } else if (right and not left) {
            buffer[position().x][position().y] = InteriorTile::water_left;
        } else {
            buffer[position().x][position().y] = InteriorTile::water_top;
        }
    }
}



void Water::render_exterior(App& app, u8 buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and (*above->metaclass())->properties() & RoomProperties::fluid) {
        buffer[position().x][position().y] = Tile::water_column;
    } else {
        u8 x = position().x;
        auto left = parent()->get_room({(u8)(x - 1), position().y});
        auto right = parent()->get_room({(u8)(x + 1), position().y});

        if (left and not right) {
            buffer[position().x][position().y] = InteriorTile::water_right;
        } else if (right and not left) {
            buffer[position().x][position().y] = InteriorTile::water_left;
        } else {
            buffer[position().x][position().y] = InteriorTile::water_top;
        }
    }
}



WaterSource::WaterSource(Island* parent, const Vec2<u8>& position)
    : Water(parent, position, name())
{
}



void WaterSource::update(Platform& pfrm, App& app, Microseconds delta)
{
    flood_timer_ += delta;

    Water::update(pfrm, app, delta);
}



void WaterSource::check_flood_parent(Platform& pfrm,
                                     App& app,
                                     Microseconds delta)
{
    decay_ = 0;
    has_flood_parent_ = false;
}



} // namespace skyland
