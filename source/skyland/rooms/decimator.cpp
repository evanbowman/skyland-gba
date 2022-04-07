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


#include "decimator.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(decimator_reload_ms);



void Decimator::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    auto secs = decimator_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_decimator)->c_str(),
                secs,
                decimator_reload_ms / 100 - secs * 10);
}



Decimator::Decimator(Island* parent, const Vec2<u8>& position)
    : Room(parent, name(), position)
{
}



void Decimator::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    const auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return;
    }

    bool has_pilot = false;
    for (auto& chr : characters()) {
        if (chr->parent() == parent()) {
            has_pilot = true;
        }
    }

    if (has_pilot and reload_ > 0) {
        reload_ -= delta;

        if (reload_ < 0) {
            if (parent() == &app.player_island()) {
                time_stream::event::PlayerRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            } else {
                time_stream::event::OpponentRoomReloadComplete e;
                e.room_x_ = position().x;
                e.room_y_ = position().y;
                app.time_stream().push(app.level_timer(), e);
            }
        }

    } else if (has_pilot) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island(app);

        if (island and not island->is_destroyed()) {
            app.camera()->shake(4);

            auto start = center();

            // This just makes it a bit less likely for cannonballs to
            // run into the player's own buildings, especially around
            // corners.
            if (island == &app.player_island()) {
                start.x -= 18;
            } else {
                start.x += 18;
            }

            auto target = center();
            if (parent() == &app.player_island()) {
                target.x += 100.f;
            } else {
                target.x -= 100.f;
            }


            auto c = app.alloc_entity<DecimatorBurst>(
                pfrm, start, target, parent(), position());

            if (c) {
                parent()->projectiles().push(std::move(c));
            }

            if (counter_ < 6) {
                ++counter_;
                reload_ += milliseconds(200);
            } else {
                reload_ += 1000 * decimator_reload_ms;
                counter_ = 0;
            }
        }
    }
}



void Decimator::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

    if (reload_ <= 0) {
        // Reloaded.
    } else if (reload_ < 1000 * decimator_reload_ms) {
        reload_ += delta;
    }
}



void Decimator::___rewind___finished_reload(Platform&, App&)
{
    reload_ = 1;
}



void Decimator::___rewind___ability_used(Platform&, App&)
{
    reload_ = 0;

    if (counter_ > 1) {
        --counter_;
    }

    // FIXME: Adjust counter correctly.
}



void Decimator::plot_walkable_zones(App& app, bool matrix[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        matrix[pos.x][pos.y + 1] = true;
    } else {
        matrix[pos.x + 1][pos.y + 1] = true;
    }
}



void Decimator::render_interior(App& app, u8 buffer[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x][pos.y] = InteriorTile::decimator_int;
    } else {
        buffer[pos.x][pos.y] = InteriorTile::decimator_1;
        buffer[pos.x][pos.y + 1] = InteriorTile::decimator_2;
        buffer[pos.x + 1][pos.y + 1] = InteriorTile::plain_floor;
        buffer[pos.x + 1][pos.y] = InteriorTile::decimator_int;
    }
}



void Decimator::render_exterior(App& app, u8 buffer[16][16])
{
    auto pos = position();

    if (parent() == &app.player_island()) {
        buffer[pos.x + 1][pos.y] = Tile::decimator_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x][pos.y] = Tile::armored_wall_1;
        buffer[pos.x][pos.y + 1] = Tile::wall_plain_2;
    } else {
        buffer[pos.x][pos.y] = Tile::decimator_1;
        buffer[pos.x][pos.y + 1] = Tile::decimator_2;
        buffer[pos.x + 1][pos.y] = Tile::armored_wall_1;
        buffer[pos.x + 1][pos.y + 1] = Tile::wall_plain_2;
    }
}



} // namespace skyland
