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



void Forcefield::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_forcefield)->c_str();
}



Forcefield::Forcefield(Island* parent,
                       const RoomCoord& position,
                       const char* name)
    : Room(parent, name, position)
{
}



void Forcefield::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

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



void Forcefield2::format_description(Platform& pfrm, StringBuffer<512>& buffer)
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



void Forcefield::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() == 0) {
        glass_break_sound.play(pfrm, 3);
    }
}



void Forcefield2::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield2::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    Room::rewind(pfrm, app, delta);

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
