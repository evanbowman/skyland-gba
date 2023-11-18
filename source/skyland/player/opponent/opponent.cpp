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


#include "opponent.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void Opponent::on_room_damaged(App& app, Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : app.birds()) {
        if (bird->island(app) == island) {
            bird->signal(app);
        }
    }
}



} // namespace skyland
