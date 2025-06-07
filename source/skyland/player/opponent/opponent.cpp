////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "opponent.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void Opponent::on_room_damaged(Room& room)
{
    auto island = room.parent();

    // Birds alerted when island attacked.
    for (auto& bird : APP.birds()) {
        if (bird->island() == island) {
            bird->signal();
        }
    }
}



} // namespace skyland
