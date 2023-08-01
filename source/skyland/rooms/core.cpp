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


#include "core.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Core::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_power_core)->c_str();
}



Core::Core(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Core::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



Sound core_destroyed("core_destroyed");



void Core::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() == 0) {
        core_destroyed.play(pfrm, 4, milliseconds(600));
        core_explosion(pfrm, app, parent(), center());
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



ScenePtr<Scene> Core::select(Platform& pfrm, App& app, const RoomCoord& cursor)
{
    if (auto scn = Room::select(pfrm, app, cursor)) {
        return scn;
    }

    if (not parent()->manufactory_count()) {
        return null_scene();
    }

    auto upgrade_to = skyland::metaclass_index("reactor");

    return scene_pool::alloc<UpgradePromptScene>(
        position(), metaclass_index(), upgrade_to);

    // TODO: should the block be upgradable to reactor, or not? In practice,
    // reactor is a different size, and a power core will always be guarded by
    // hull, so it doesn't make tons of sense to have a shortcut for upgrading,
    // because other blocks will always be in the way...

    return null_scene();
}



Power BackupCore::power_usage(App& app) const
{
    auto base_power = Core::power_usage(app);

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



void BackupCore::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    for (auto& room : parent()->rooms()) {
        if ((*room->metaclass())->category() == Room::Category::power) {
            if (room.get() not_eq this and room->metaclass() == metaclass()) {

                // One allowed per island.
                if (length(characters()) < length(room->characters())) {
                    apply_damage(pfrm, app, Room::health_upper_limit());
                } else {
                    room->apply_damage(pfrm, app, Room::health_upper_limit());
                }

                return;
            }
        }
    }
}



void BackupCore::format_description(Platform& pfrm, StringBuffer<512>& buffer)
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
