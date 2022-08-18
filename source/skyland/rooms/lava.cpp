////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#include "lava.hpp"
#include "globals.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"
#include "water.hpp"



namespace skyland
{



Lava::Lava(Island* parent, const RoomCoord& position, const char* name)
    : Room(parent, name, position)
{
}



void Lava::check_flood_parent(Platform& pfrm, App& app, Microseconds delta)
{
    // NOTE: we want to destroy a lava block if its parent lava block no
    // longer exists.

    has_flood_parent_ = false;

    bool flood_source_is_lava = false;

    if (auto room = parent()->get_room(flood_parent_)) {
        Lava* l = room->cast<Lava>();
        if (not l) {
            l = room->cast<LavaSource>();
        }

        if (l) {
            flood_source_is_lava = true;
            if (l->has_flood_parent_) {
                has_flood_parent_ = true;
            } else if (l->cast<LavaSource>()) {
                has_flood_parent_ = true;
            }
        }
    }

    if (not flood_source_is_lava and not has_flood_parent_) {
        decay_ += delta;

        if (decay_ > milliseconds(1000)) {
            __set_health(0);
        }
    } else {
        decay_ = 0;
    }
}



void Lava::update(Platform& pfrm, App& app, Microseconds delta)
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

    damage_timer_ += delta;
    if (damage_timer_ > milliseconds(400)) {
        damage_timer_ = 0;

        bool solidify = false;

        auto damage = [&](u8 x, u8 y) {
            if (auto room = parent()->get_room({x, y})) {
                if (not room->cast<Lava>() and
                    not str_eq(room->name(), "barrier") and
                    not str_eq(room->name(), "masonry")) {

                    auto props = (*room->metaclass())->properties();

                    if (not parent()->fire_present({x, y}) and
                        not(props & RoomProperties::fireproof)) {
                        parent()->fire_create(pfrm, app, {x, y});
                    }

                    if (props & RoomProperties::fluid) {
                        // This cast should happen only once. If we're a
                        // non-lava fluid, water's sort of the only other
                        // option.
                        if (room->cast<Water>()) {
                            solidify = true;
                            if (position().y <= y) {
                                room->apply_damage(pfrm, app, 9999);
                            }
                        }
                    }
                }
            }
        };

        const auto x = position().x;
        const auto y = position().y;

        if (x > 0) {
            damage(x - 1, y);
        }

        damage(x + 1, y);
        damage(x, y + 1);
        damage(x, y - 1);

        if (solidify) {
            __unsafe__transmute(pfrm, app, skyland::metaclass_index("basalt"));
            return;
        }
    }

    if (flood_timer_ >= milliseconds(1000)) {
        flood_timer_ -= milliseconds(1000);

        auto flood = [&](u8 x, u8 y) {
            (*load_metaclass("lava"))
                ->create(pfrm, app, parent(), {x, y}, false);

            parent()->schedule_repaint();

            if (auto room = parent()->get_room({x, y})) {
                if (auto w = room->cast<Lava>()) {
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



void Lava::render_interior(App& app, TileId buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and (*above->metaclass())->properties() & RoomProperties::fluid) {
        buffer[position().x][position().y] = InteriorTile::lava_column;
    } else {
        u8 x = position().x;
        auto left = parent()->get_room({(u8)(x - 1), position().y});
        auto right = parent()->get_room({(u8)(x + 1), position().y});

        if (left and not right) {
            buffer[position().x][position().y] = InteriorTile::lava_right;
        } else if (right and not left) {
            buffer[position().x][position().y] = InteriorTile::lava_left;
        } else {
            buffer[position().x][position().y] = InteriorTile::lava_top;
        }
    }
}



void Lava::render_exterior(App& app, TileId buffer[16][16])
{
    auto above = parent()->get_room({position().x, (u8)(position().y - 1)});
    if (above and (*above->metaclass())->properties() & RoomProperties::fluid) {
        buffer[position().x][position().y] = Tile::lava_column;
    } else {
        u8 x = position().x;
        auto left = parent()->get_room({(u8)(x - 1), position().y});
        auto right = parent()->get_room({(u8)(x + 1), position().y});

        if (left and not right) {
            buffer[position().x][position().y] = InteriorTile::lava_right;
        } else if (right and not left) {
            buffer[position().x][position().y] = InteriorTile::lava_left;
        } else {
            buffer[position().x][position().y] = InteriorTile::lava_top;
        }
    }
}



LavaSource::LavaSource(Island* parent, const RoomCoord& position)
    : Lava(parent, position, name())
{
}



void LavaSource::update(Platform& pfrm, App& app, Microseconds delta)
{
    flood_timer_ += delta;

    Lava::update(pfrm, app, delta);
}



void LavaSource::check_flood_parent(Platform& pfrm,
                                    App& app,
                                    Microseconds delta)
{
    decay_ = 0;
    has_flood_parent_ = false;
}



} // namespace skyland
