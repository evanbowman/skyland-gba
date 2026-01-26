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


#include "opponent/enemyAI.hpp"
#include "skyland/island.hpp"



// The battle sandbox mode has a spectator mode, allowing the player to step
// back and let an AI player take control of both castles.



namespace skyland
{



class SandboxSpectatorPlayer : public EnemyAI
{
public:
    SandboxSpectatorPlayer() : EnemyAI(&player_island(), opponent_island())
    {
    }


    void update(Time delta) override;


    bool button_down(Button k) override;


    bool button_up(Button k) override;


    bool button_pressed(Button k) override;


    bool button_held(Button k, Time duration) override;


    void button_held_reset(Button k, Time decrement) override;


    void button_held_distribute(const Button* include_list) override;


private:
    Time button_held_timers_[static_cast<int>(Button::count)];
};



} // namespace skyland
