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


#include "friendlyAI.hpp"
#include "enemyAI.hpp"
#include "skyland/scene/scriptHookScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void FriendlyAI::on_room_damaged(Platform& pfrm, App& app, Room& room)
{
    Opponent::on_room_damaged(pfrm, app, room);

    // What!? The player attacked us! We're no longer a friendly AI.
    app.swap_opponent<EnemyAI>();

    // You attacked a neutral opponent! Not good!
    app.score().set(0);
}



} // namespace skyland
