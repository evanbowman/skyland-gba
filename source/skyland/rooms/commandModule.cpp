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
      id_buffers_(allocate_dynamic<IdBuffers>("command-module-buffer"))
{
}



void CommandModule::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_command_module)->c_str();
}



void CommandModule::update(Microseconds delta)
{
    Room::update(delta);

    Room::ready();

    if (&parent()->owner() == &APP.opponent()) {
        if (APP.game_mode() == App::GameMode::multiplayer) {
            // While we don't want players placing a command module in opponent
            // castles in the sandbox mode, we still want to allow this block's
            // existence in opponent's castles in multiplayer mode, although it
            // shouldn't do anything as the block on the connected device is
            // already orchestrating character movement.
            return;
        } else {
            apply_damage(Room::health_upper_limit());
            return;
        }
    }

    if (APP.game_mode() == App::GameMode::co_op) {
        // Unsupported in co-op mode.
        apply_damage(Room::health_upper_limit());
    }

    if (APP.opponent().is_friendly()) {
        return;
    }

    next_action_timer_ -= delta;
    if (next_action_timer_ <= 0) {
        next_action_timer_ = milliseconds(1500);

        if (id_buffers_->local_.empty() or
            local_buffer_index_ >= id_buffers_->local_.size()) {
            local_buffer_index_ = 0;
            id_buffers_->local_.clear();

            for (auto& room : APP.player_island().rooms()) {
                for (auto& chr : room->characters()) {
                    if (chr->owner() == &APP.player() and
                        not chr->co_op_locked()) {
                        id_buffers_->local_.push_back(chr->id());
                    }
                }
            }
        }

        if (not id_buffers_->local_.empty()) {
            auto chr_id = (id_buffers_->local_)[local_buffer_index_++];
            auto info = APP.player_island().find_character_by_id(chr_id);
            if (info.first and info.first->ai_automated()) {
                EnemyAI::assign_local_character(*info.first,
                                                &APP.player(),
                                                &APP.player_island(),
                                                APP.opponent_island(),
                                                true);
            }
        }

        if (id_buffers_->boarded_.empty() or
            boarded_buffer_index_ >= id_buffers_->boarded_.size()) {
            boarded_buffer_index_ = 0;
            id_buffers_->boarded_.clear();

            if (APP.opponent_island()) {
                for (auto& room : APP.opponent_island()->rooms()) {
                    for (auto& chr : room->characters()) {
                        if (chr->owner() == &APP.player() and
                            not chr->co_op_locked()) {
                            id_buffers_->boarded_.push_back(chr->id());
                        }
                    }
                }
            }
        }

        if (not id_buffers_->boarded_.empty()) {
            auto chr_id = (id_buffers_->boarded_)[boarded_buffer_index_++];
            if (APP.opponent_island()) {
                auto info = APP.opponent_island()->find_character_by_id(chr_id);
                if (info.first and info.first->ai_automated()) {
                    EnemyAI::assign_boarded_character(*info.first,
                                                      &APP.player(),
                                                      &APP.player_island(),
                                                      APP.opponent_island());
                }
            }
        }
    }

    if (room_check_index_ >= player_island().rooms().size()) {
        room_check_index_ = 0;
    } else {
        auto& room = *player_island().rooms()[room_check_index_++];
        if (&room not_eq this and room.metaclass() == this->metaclass()) {
            // Player built two command modules.
            room.apply_damage(Room::health_upper_limit());
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
