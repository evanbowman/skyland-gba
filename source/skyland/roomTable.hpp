////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "room.hpp"



namespace skyland
{


struct RoomMatrix
{
    Room* data_[16][16];
};


template <u32 room_count, int map_width> class RoomTable
{
public:
    RoomTable(RoomMatrix* data) : data_(data)
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

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                data_->data_[x][y] = nullptr;
            }
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

        return data_->data_[coord.x][coord.y];
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


    using IndexType = u16;


    // re_sort parameter: When erasing a room, the rooms_ buffer remains sorted.
    void reindex(bool re_sort)
    {
        if (re_sort) {
            std::sort(rooms_.begin(), rooms_.end(), [](auto& lhs, auto& rhs) {
                return lhs->position().x < rhs->position().x;
            });
        }

        for (int x = 0; x < 16; ++x) {
            for (int y = 0; y < 16; ++y) {
                data_->data_[x][y] = nullptr;
            }
        }

        for (auto& room : rooms_) {
            if (room->hidden()) {
                continue;
            }
            for (int x = room->position().x;
                 x < room->position().x + room->size().x;
                 ++x) {
                for (int y = room->position().y;
                     y < room->position().y + room->size().y;
                     ++y) {
                    if (x < 16 and y < 16) {
                        data_->data_[x][y] = room.get();
                    }
                }
            }
        }
    }

private:
    RoomMatrix* data_;
    Rooms rooms_;
};



} // namespace skyland
