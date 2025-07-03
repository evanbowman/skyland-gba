////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "hull.hpp"
#include "mirrorHull.hpp"
#include "platform/platform.hpp"
#include "poweredHull.hpp"
#include "skyland/achievement.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/tile.hpp"
#include "stackedHull.hpp"



namespace skyland
{



Optional<Room::UpgradeList> Hull::upgrade_mt_list() const
{
    UpgradeList upgrades;
    upgrades.push_back(skyland::metaclass_index(PoweredHull::name()));
    if (achievements::is_unlocked(achievements::Achievement::ship_of_theseus)) {
        upgrades.push_back(skyland::metaclass_index(MirrorHull::name()));
    }
    upgrades.push_back(skyland::metaclass_index(StackedHull::name()));
    return upgrades;
}



void Hull::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_hull)->c_str();
}



Hull::Hull(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



SHARED_VARIABLE(block_crack_threshold_health);



TileId Hull::tile() const
{
    if (health() <= block_crack_threshold_health) {
        return Tile::damaged_hull;
    } else {
        return Tile::hull;
    }
}



void Hull::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Hull::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Hull::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void Hull::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



} // namespace skyland
