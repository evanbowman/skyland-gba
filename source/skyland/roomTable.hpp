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


#pragma once

#include "room.hpp"



namespace skyland
{



// NOTE: SKYLAND uses a 16x16 map grid for each island. But most rooms in fact
// take up more than one grid square, e.g. a workshop room takes up 2x2
// squares. Storing a table of 16*16 pointers to rooms would make room access by
// grid coordinate most efficient, but would take up more memory, and if you
// wanted to iterate over all unique rooms, the code would become potentially
// computationally intensive (even if only three rooms, you would need to
// iterate over a 16x16 grid, and keep track of which rooms that you'd seen
// before, in case a room spans multiple grid coordinates). For these reasons
// and more, SKYLAND stores rooms in a flat vector, and uses various techniques
// to speed up access by x,y coordinates.



template <u32 room_count, int map_width> class RoomTable
{
public:
    RoomTable()
    {
        reindex(true);
    }


    using Rooms = Buffer<RoomPtr<Room>, room_count>;


    RoomPtr<Room>& operator[](int index)
    {
        return rooms_[index];
    }


    bool full() const
    {
        return rooms_.full();
    }


    typename Rooms::Iterator begin()
    {
        return rooms_.begin();
    }


    typename Rooms::Iterator end()
    {
        return rooms_.end();
    }


    typename Rooms::Iterator begin() const
    {
        return rooms_.begin();
    }


    typename Rooms::Iterator end() const
    {
        return rooms_.end();
    }


    typename Rooms::Iterator erase(typename Rooms::Iterator it)
    {
        auto result = rooms_.erase(it);
        reindex(false);
        return result;
    }


    void clear()
    {
        rooms_.clear();

        for (auto& index : x_jump_table_) {
            index = 0;
        }
    }


    bool insert_room(RoomPtr<Room> room)
    {
        bool result = rooms_.push_back(std::move(room));
        reindex(true);
        return result;
    }


    Room* get_room(const RoomCoord& coord)
    {
        if (coord.x >= map_width or coord.y >= 15) {
            return nullptr;
        }

        // We skip to the known location of where rooms with a given
        // x-coordinate start in the buffer, so we essentially have bins of
        // rooms by x coordinate.
        u32 i = x_jump_table_[coord.x];
        for (; i < rooms_.size(); ++i) {
            Room* room = rooms_[i].get();
            if (not room->hidden() and coord.x >= room->position().x and
                coord.y >= room->position().y and
                coord.x < room->position().x + room->size().x and
                coord.y < room->position().y + room->size().y) {

                return room;
            }
        }

        return nullptr;
    }


    void remove_room(Room* room)
    {
        for (auto it = rooms_.begin(); it not_eq rooms_.end();) {
            if (it->get() == room) {
                it = rooms_.erase(it);
                break;
            } else {
                ++it;
            }
        }

        reindex(false);
    }


    u32 size() const
    {
        return rooms_.size();
    }


private:
    using IndexType = u16;


    // re_sort parameter: When erasing a room, the rooms_ buffer remains sorted.
    void reindex(bool re_sort)
    {
        if (rooms_.empty()) {
            for (auto& elem : x_jump_table_) {
                elem = 0;
            }
            return;
        }

        static const auto uninit_index = std::numeric_limits<IndexType>::max();

        for (auto& elem : x_jump_table_) {
            // Initialize to an arbitrarily high number.
            elem = uninit_index;
        }

        if (re_sort) {
            std::sort(rooms_.begin(), rooms_.end(), [](auto& lhs, auto& rhs) {
                return lhs->position().x < rhs->position().x;
            });
        }

        for (u32 i = 0; i < rooms_.size(); ++i) {
            int room_min_x = rooms_[i]->position().x;

            for (int x = room_min_x; x < room_min_x + rooms_[i]->size().x;
                 ++x) {
                if (x_jump_table_[x] > i) {
                    x_jump_table_[x] = i;
                }
            }
        }

        for (int i = 0; i < map_width; ++i) {
            if (x_jump_table_[i] == uninit_index) {
                if (i > 0) {
                    x_jump_table_[i] = x_jump_table_[i - 1];
                } else {
                    x_jump_table_[i] = 0;
                }
            }
        }
    }


    // The room table stores rooms in a buffer, and for faster access, sorts
    // rooms in the buffer by x, and keeps track of the index of all room x
    // coordinates. So if you wanted a room with x coordinate 5, you would start
    // iterating over rooms_ at index x_jump_table_[5].
    IndexType x_jump_table_[map_width];
    Rooms rooms_;
};



} // namespace skyland