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


#include "bulkhead.hpp"
#include "skyland/island.hpp"
#include "skyland/network.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



void Bulkhead::plot_walkable_zones(bool matrix[16][16],
                                   BasicCharacter* for_character)
{
    // If the door belongs to the character's home island or the door is
    // currently open, then a character can walk through it.
    if (for_character and for_character->owner() == &parent()->owner()) {
        Room::plot_walkable_zones(matrix, for_character);
    } else if (open_) {
        Room::plot_walkable_zones(matrix, for_character);
    }
}



void Bulkhead::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_bulkhead_door)->c_str();
}



Bulkhead::Bulkhead(Island* parent, const RoomCoord& position)
    : Room(parent, name(), position)
{
}



void Bulkhead::update(Microseconds delta)
{
    Room::update(delta);

    Room::ready();

    if (length(characters())) {
        set_open(true);
    } else if (parent()->power_supply() < parent()->power_drain()) {
        set_open(true);
    } else {
        auto pos = position();
        pos.y += 1;
        // We have to run the code below, otherwise, the door would only open
        // when a character has finished walking into the slot that the room
        // occupies. So a character would walk into a door, then it would open,
        // then the character would walk out. This consumes a bit of extra cpu,
        // but looks better.
        pos.x -= 1;
        bool chr_moving_in = false;
        if (auto left = parent()->get_room(pos)) {
            pos.x += 1;
            for (auto& chr : left->characters()) {
                if (chr->get_movement_path() and
                    chr->get_movement_path()->back() == pos) {
                    chr_moving_in = true;
                    break;
                }
            }
        } else {
            pos.x += 1;
        }
        pos.x += 1;
        if (auto right = parent()->get_room(pos)) {
            pos.x -= 1;
            for (auto& chr : right->characters()) {
                if (chr->get_movement_path() and
                    chr->get_movement_path()->back() == pos) {
                    chr_moving_in = true;
                    break;
                }
            }
        } else {
            pos.x -= 1;
        }
        pos.y += 1;
        if (auto down = parent()->get_room(pos)) {
            pos.y -= 1;
            for (auto& chr : down->characters()) {
                if (chr->get_movement_path() and
                    chr->get_movement_path()->back() == pos) {
                    chr_moving_in = true;
                    break;
                }
            }
        }

        set_open(chr_moving_in);
    }
}



void Bulkhead::render_interior(App* app, TileId buffer[16][16])
{
    if (open_) {
        buffer[position().x][position().y] = InteriorTile::bulkhead_open_1;
        buffer[position().x][position().y + 1] = InteriorTile::plain_floor;
    } else {
        buffer[position().x][position().y] = InteriorTile::bulkhead_closed_1;
        buffer[position().x][position().y + 1] =
            InteriorTile::bulkhead_closed_2;
    }

    interior_visible_ = true;
}



void Bulkhead::___rewind___finished_reload()
{
    set_open(not open_);
}



void Bulkhead::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::wall_plain_1;
    buffer[position().x][position().y + 1] = Tile::wall_plain_2;

    interior_visible_ = false;
}



void Bulkhead::set_open(bool open)
{
    if (open_ == open) {
        return;
    }

    if (is_player_island(parent())) {
        time_stream::event::PlayerRoomReloadComplete e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentRoomReloadComplete e;
        e.room_x_ = position().x;
        e.room_y_ = position().y;
        APP.time_stream().push(APP.level_timer(), e);
    }

    if (APP.time_stream().pushes_enabled()) {
        if (not PLATFORM.speaker().is_sound_playing("door")) {
            PLATFORM.speaker().play_sound("door", 0);
        }
    }

    open_ = open;

    if (&parent()->owner() == &APP.player()) {
        network::packet::OpponentBulkheadChanged packet;
        packet.room_x_ = position().x;
        packet.room_y_ = position().y;
        packet.open_ = open_;
        network::transmit(packet);
    }

    if (parent()->interior_visible()) {
        schedule_repaint();
    }

    parent()->on_layout_changed({position().x, u8(position().y + 1)});
}



} // namespace skyland
