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


#include "opponent/enemyAI.hpp"
#include "skyland/island.hpp"



// The battle sandbox mode has a spectator mode, allowing the player to step
// back and let an AI player take control of both castles.



namespace skyland
{



class SandboxSpectatorPlayer : public EnemyAI
{
public:
    SandboxSpectatorPlayer(App& app)
        : EnemyAI(&player_island(app), opponent_island(app))
    {
    }


    void update(App& app, Microseconds delta) override;


    bool key_down(Key k) override;


    bool key_up(Key k) override;


    bool key_pressed(Key k) override;


    bool key_held(Key k, Microseconds duration) override;


    void key_held_reset(Key k, Microseconds decrement) override;


    void key_held_distribute(const Key* include_list) override;


private:
    Microseconds key_held_timers_[static_cast<int>(Key::count)];
};



} // namespace skyland
