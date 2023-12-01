////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to the SKYLAND,
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



void Forcefield::update(Microseconds delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield::rewind(Microseconds delta)
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



void Forcefield2::update(Microseconds delta)
{
    Room::update(delta);

    if (last_tile_ not_eq tile()) {
        schedule_repaint();
    }
}



void Forcefield2::rewind(Microseconds delta)
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
