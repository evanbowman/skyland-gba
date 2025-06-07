////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "forcefield.hpp"
#include "platform/platform.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



const char* Forcefield::upgrade_mt_name() const
{
    return Forcefield2::name();
}



const char* Forcefield2::upgrade_mt_name() const
{
    return nullptr;
}



void Forcefield::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_forcefield)->c_str();
}



Forcefield::Forcefield(Island* parent,
                       const RoomCoord& position,
                       const char* name)
    : Room(parent, name, position)
{
}



void Forcefield::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void Forcefield::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void Forcefield2::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_forcefield2)->c_str();
}



void Forcefield2::render_interior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



void Forcefield2::render_exterior(App* app, TileId buffer[16][16])
{
    last_tile_ = tile();
    buffer[position().x][position().y] = last_tile_;
}



Sound glass_break_sound("glass_break");



void Forcefield::finalize()
{
    Room::finalize();

    if (health() == 0) {
        glass_break_sound.play(3);
    }
}



void Forcefield2::update(Time delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield2::rewind(Time delta)
{
    Room::rewind(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



extern SharedVariable block_crack_threshold_health;



TileId Forcefield::tile() const
{
    if (health() <= block_crack_threshold_health) {
        return Tile::damaged_forcefield;
    } else {
        return Tile::forcefield;
    }
}



TileId Forcefield2::tile() const
{
    if (health() <= block_crack_threshold_health) {
        return Tile::damaged_forcefield2;
    } else {
        return Tile::forcefield2;
    }
}



} // namespace skyland
