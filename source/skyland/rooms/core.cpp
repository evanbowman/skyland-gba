////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "core.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



const char* Core::upgrade_mt_name() const
{
    if (APP.faction() == Faction::goblin) {
        return "chaos-core";
    } else if (APP.faction() == Faction::sylph) {
        return nullptr;
    }
    return "reactor";
}



void Core::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_power_core)->c_str();
}



Core::Core(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Core::update(Time delta)
{
    Room::update(delta);
}



Sound core_destroyed("core_destroyed.raw");



void Core::finalize()
{
    Room::finalize();

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(), center());
    }
}



void Core::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::core_1;
    buffer[position().x][position().y + 1] = InteriorTile::core_2;
    buffer[position().x + 1][position().y] = InteriorTile::core_3;
    buffer[position().x + 1][position().y + 1] = InteriorTile::core_4;
}



void Core::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



Power BackupCore::power_usage() const
{
    auto base_power = Core::power_usage();

    if (health() == 0) {
        return 0;
    }

    for (auto& room : parent()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::power) {
            if (room.get() not_eq this) {
                // Another power-generating structure exists.
                return 0;
            }
        }
    }
    return base_power;
}



const char* BackupCore::upgrade_mt_name() const
{
    return "power-core";
}



void BackupCore::update(Time delta)
{
    Room::update(delta);

    for (auto& room : parent()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::power) {
            if (room.get() not_eq this and room->metaclass() == metaclass()) {

                // One allowed per island.
                if (length(characters()) < length(room->characters())) {
                    apply_damage(Room::health_upper_limit());
                } else {
                    room->apply_damage(Room::health_upper_limit());
                }

                return;
            }
        }
    }
}



void BackupCore::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_backup_core)->c_str();
}



void BackupCore::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::core_1;
    buffer[position().x][position().y + 1] = InteriorTile::core_2;
    buffer[position().x + 1][position().y] = InteriorTile::backup_core;
    buffer[position().x + 1][position().y + 1] = InteriorTile::core_4;
}



} // namespace skyland
