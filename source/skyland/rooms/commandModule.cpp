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
      local_chrs_(allocate_dynamic<IdBuffer>("command-module-buffer"))
{
}



void CommandModule::format_description(Platform& pfrm,
                                       StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_command_module)->c_str();
}



void CommandModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    Room::update(pfrm, app, delta);

    Room::ready();

    if (&parent()->owner() == &app.opponent()) {
        apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }

    if (app.opponent().is_friendly()) {
        return;
    }

    next_action_timer_ -= delta;
    if (next_action_timer_ <= 0) {
        next_action_timer_ = milliseconds(1500);

        if (local_chrs_->empty() or buffer_index_ >= local_chrs_->size()) {
            buffer_index_ = 0;
            local_chrs_->clear();

            for (auto& room : app.player_island().rooms()) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &app.player() and
                        not chr->co_op_locked()) {
                        local_chrs_->push_back(chr->id());
                    }
                }
            }
        }

        if (not local_chrs_->empty()) {
            auto chr_id = (*local_chrs_)[buffer_index_++];
            auto info = app.player_island().find_character_by_id(chr_id);
            if (info.first) {
                EnemyAI::assign_local_character(pfrm,
                                                app,
                                                *info.first,
                                                &app.player(),
                                                &app.player_island(),
                                                app.opponent_island());
            }
        }
    }

    if (room_check_index_ >= player_island(app).rooms().size()) {
        room_check_index_ = 0;
    } else {
        auto& room = *player_island(app).rooms()[room_check_index_++];
        if (&room not_eq this and room.metaclass() == this->metaclass()) {
            // Player built two command modules.
            room.apply_damage(pfrm, app, Room::health_upper_limit());
        }
    }
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
