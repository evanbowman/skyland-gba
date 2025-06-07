////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "commandModule.hpp"
#include "platform/platform.hpp"
#include "skyland/island.hpp"
#include "skyland/player/opponent/enemyAI.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStream.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



CommandModule::CommandModule(Island* parent,
                             const RoomCoord& position,
                             const char* n)
    : Room(parent, n, position),
      id_buffers_(allocate_dynamic<IdBuffers>("command-module-buffer"))
{
}



void CommandModule::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_command_module)->c_str();
}



void CommandModule::update(Time delta)
{
    Room::update(delta);

    // Deprecated and removed
    apply_damage(Room::health_upper_limit());
}



void CommandModule::render_interior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = InteriorTile::command_module_1;
    buffer[x][y + 1] = InteriorTile::command_module_2;
}



void CommandModule::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_window_1;
    buffer[position().x][position().y + 1] = Tile::wall_window_2;
}



} // namespace skyland
