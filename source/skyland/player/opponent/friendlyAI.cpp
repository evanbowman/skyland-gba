////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "friendlyAI.hpp"
#include "enemyAI.hpp"
#include "skyland/rooms/bell.hpp"
#include "skyland/scene/scriptHookScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void FriendlyAI::on_room_damaged(Room& room)
{
}



void FriendlyAI::update(Time delta)
{
}



void FriendlyAI::on_level_start()
{
    APP.with_opponent_island([](Island& isle) {
        for (auto& room : isle.rooms()) {
            if (auto b = room->cast<Bell>()) {
                b->schedule_chimes(seconds(1), 3, 0, milliseconds(1500));
            }
        }
    });
}



} // namespace skyland
