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
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/projectile/decimatorBurst.hpp"
#include "skyland/scene/weaponSetTargetScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(decimator_reload_ms);



void Decimator::format_description(StringBuffer<512>& buffer)
{
    auto secs = decimator_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_decimator)->c_str(),
                secs,
                decimator_reload_ms / 100 - secs * 10);
}



Decimator::Decimator(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Decimator::unset_target()
{
    reload_ = 1000 * decimator_reload_ms;
    counter_ = 0;
}



void Decimator::update(Microseconds delta)
{
    Room::update(delta);

    Room::ready();

    const auto& mt_prep_seconds = globals().multiplayer_prep_seconds_;

    if (mt_prep_seconds) {
        return;
    }

    bool has_pilot = false;
    for (auto& chr : characters()) {
        if (chr->parent() == parent()) {
            has_pilot = true;
        }
    }

    if (not APP.opponent_island()) {
        return;
    }

    const bool opponent_friendly =
        parent() == &APP.player_island() and
        static_cast<Opponent&>(APP.opponent_island()->owner()).is_friendly();


    if (has_pilot and reload_ > 0) {

        if (not opponent_friendly) {
            reload_ -= delta;

            if (reload_ < 0) {
                if (parent() == &APP.player_island()) {
                    time_stream::event::PlayerRoomReloadComplete e;
                    e.room_x_ = position().x;
                    e.room_y_ = position().y;
                    APP.time_stream().push(APP.level_timer(), e);
                } else {
                    time_stream::event::OpponentRoomReloadComplete e;
                    e.room_x_ = position().x;
                    e.room_y_ = position().y;
                    APP.time_stream().push(APP.level_timer(), e);
                }
            }
        }

    } else if (has_pilot) {

        if (parent()->power_supply() < parent()->power_drain()) {
            return;
        }

        auto island = other_island();

        if (island and not island->is_destroyed()) {
            APP.camera()->shake(4);

            auto start = center();

            // This just makes it a bit less likely for cannonballs to
            // run into the player's own buildings, especially around
            // corners.
            if (island == &APP.player_island()) {
                start.x -= 18.0_fixed;
            } else {
                start.x += 18.0_fixed;
            }

            auto target = center();
            if (parent() == &APP.player_island()) {
                target.x += 100.0_fixed;
            } else {
                target.x -= 100.0_fixed;
            }


            auto c = APP.alloc_entity<DecimatorBurst>(
                start, target, parent(), position());

            if (c) {
                parent()->projectiles().push(std::move(c));
                set_ai_aware(true);
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



void Decimator::rewind(Microseconds delta)
{
    Room::rewind(delta);

    if (reload_ <= 0) {
        // Reloaded.
    } else if (reload_ < 1000 * decimator_reload_ms) {
        reload_ += delta;
    }
}



void Decimator::___rewind___finished_reload()
{
    reload_ = 1;
}



void Decimator::___rewind___ability_used()
{
    reload_ = 0;

    if (counter_ > 1) {
        --counter_;
    }

    // FIXME: Adjust counter correctly.
}



void Decimator::plot_walkable_zones(bool matrix[16][16],
                                    BasicCharacter* for_character)
{
    auto pos = position();

    if (parent() == &APP.player_island()) {
        matrix[pos.x][pos.y + 1] = true;
    } else {
        matrix[pos.x + 1][pos.y + 1] = true;
    }
}



void Decimator::render_interior(App* app, TileId buffer[16][16])
{
    auto pos = position();

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (right) {
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



void Decimator::render_exterior(App* app, TileId buffer[16][16])
{
    auto pos = position();

    bool right = false;
    if (app) {
        right = parent() == &app->player_island();
    }

    if (right) {
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



void Decimator::finalize()
{
    Room::finalize();

    if (health() <= 0) {
        ExploSpawner::create(center());
    }
}



} // namespace skyland
