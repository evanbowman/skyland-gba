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


#include "stairwell.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/upgradePromptScene.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



const char* Stairwell::upgrade_mt_name() const
{
    return "stairwell+";
}



void Stairwell::format_description(Platform& pfrm, StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_stairwell)->c_str();
}



Stairwell::Stairwell(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void Stairwell::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);
}



void Stairwell::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_base;
}



void Stairwell::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 3] = Tile::wall_window_2;
}



void Stairwell::plot_walkable_zones(App& app,
                                    bool matrix[16][16],
                                    BasicCharacter* for_character)
{
    // All tiles in a stairwell are walkable, that's kind of the point.
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



void Stairwell::finalize(Platform& pfrm, App& app)
{
    Room::finalize(pfrm, app);

    if (health() <= 0) {
        auto pos = center();
        pos.y += 16.0_fixed;
        ExploSpawner::create(pfrm, app, pos);
        pos.y -= 32.0_fixed;
        ExploSpawner::create(pfrm, app, pos);
    }
}



const char* StairwellPlus::upgrade_mt_name() const
{
    return "stairwell++";
}



void StairwellPlus::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 4] = InteriorTile::ladder_base;
}



void StairwellPlus::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 3] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 4] = Tile::wall_plain_2;
}



StairwellPlus::StairwellPlus(Island* parent, const RoomCoord& position)
    : Stairwell(parent, position, name())
{
}



void StairwellPlus::plot_walkable_zones(App& app,
                                        bool matrix[16][16],
                                        BasicCharacter* for_character)
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



void StairwellPlusPlus::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::ladder_top;
    buffer[position().x][position().y + 1] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 2] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 4] = InteriorTile::ladder_mid;
    buffer[position().x][position().y + 5] = InteriorTile::ladder_base;
}



void StairwellPlusPlus::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 2] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 3] = Tile::wall_window_middle_2;
    buffer[position().x][position().y + 4] = Tile::wall_window_middle_1;
    buffer[position().x][position().y + 5] = Tile::wall_window_2;
}



StairwellPlusPlus::StairwellPlusPlus(Island* parent, const RoomCoord& position)
    : Stairwell(parent, position, name())
{
}



void StairwellPlusPlus::plot_walkable_zones(App& app,
                                            bool matrix[16][16],
                                            BasicCharacter* for_character)
{
    for (int y = 0; y < size().y; ++y) {
        matrix[position().x][position().y + y] = true;
    }
}



const char* StairwellPlusPlus::upgrade_mt_name() const
{
    return nullptr;
}



void StairwellPlus::format_description(Platform& pfrm,
                                       StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_stairwell_plus)->c_str();
}



void StairwellPlusPlus::format_description(Platform& pfrm,
                                           StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_stairwell_plus_plus)->c_str();
}



} // namespace skyland
