////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "overdriveCore.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



static const Fixnum overdrive_shutdown_ratio = 0.75_fixed;



Health OverdriveCore::overload_threshold(Health max_hp)
{
    return (Fixnum::from_integer(max_hp) * overdrive_shutdown_ratio)
        .as_integer();
}



bool OverdriveCore::is_overloaded() const
{
    return health() <= overload_threshold(max_health());
}



void OverdriveCore::format_description(StringBuffer<512>& buffer)
{
    const auto max_hp = (*load_metaclass(OverdriveCore::name()))->full_health();
    buffer += format<256>(SYS_CSTR(description_overdrive_core),
                          (overdrive_shutdown_ratio * 100.0_fixed).as_integer(),
                          "%",
                          OverdriveCore::overload_threshold(max_hp))
                  .c_str();
}



OverdriveCore::OverdriveCore(Island* parent,
                             const RoomCoord& position,
                             const char* n)
    : Room(parent, n, position)
{
}



extern Sound core_destroyed;



void OverdriveCore::update(Time delta)
{
    Room::update(delta);

    if (is_overloaded()) {
        Room::ready();
    } else {
        if (is_powered_down()) {
            set_powerdown(false);
            PLATFORM.speaker().play_sound("poweron.raw", 4);
        }
    }
}



void OverdriveCore::apply_damage(Health damage, const DamageConfiguration& conf)
{
    const bool was_overloaded = is_overloaded();

    Room::apply_damage(damage, conf);

    if (is_overloaded() and not was_overloaded) {
        set_powerdown(true);
        PLATFORM.speaker().play_sound("powerdown.raw", 4);
    }
}



Optional<Room::UpgradeList> OverdriveCore::upgrade_mt_list() const
{
    return nullopt();
}



void OverdriveCore::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::overdrive_core_1;
    buffer[position().x][position().y + 1] = InteriorTile::core_2;
    buffer[position().x + 1][position().y] = InteriorTile::overdrive_core_2;
    buffer[position().x + 1][position().y + 1] = InteriorTile::core_4;
}



void OverdriveCore::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
    buffer[position().x + 1][position().y] = Tile::wall_plain_1;
    buffer[position().x + 1][position().y + 1] = Tile::wall_plain_2;
}



} // namespace skyland
