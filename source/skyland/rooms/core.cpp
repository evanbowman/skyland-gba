////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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



void Core::update(Microseconds delta)
{
    Room::update(delta);
}



Sound core_destroyed("core_destroyed");



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
    return nullptr;
}



void BackupCore::update(Microseconds delta)
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
