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


#include "allocator.hpp"
#include "memory/buffer.hpp"
#include "player.hpp"
#include "skyland/characterId.hpp"



namespace skyland
{



// An implementation of Player, controlled directly by a player via the device's
// physical buttons.



class PlayerP1 : public Player
{
public:
    PlayerP1();


    void update(Time delta) override;


    void on_room_destroyed(Room& room) override;


    void on_room_damaged(Room& room) override;


    void on_room_plundered(Room& room) override;


    bool key_down(Key k) override;


    bool key_up(Key k) override;


    bool key_pressed(Key k) override;


    bool key_held(Key k, Time duration) override;


    void key_held_reset(Key k, Time decrement) override;


    void key_held_distribute(const Key* include_list) override;


    void on_level_start() override;


    void update_weapon_targets(Time delta) override;
    void reassign_all_weapon_targets() override;


    void delay_autofire(Time duration) override;


    void delay_crew_automation(Time duration) override;


    static void autoassign_weapon_target(Room& r);
    static void autoassign_drone_target(Drone& d);

protected:
    virtual void update_ai(Time delta);


private:
    struct AIState
    {
        using IdBuffer = Buffer<CharacterId, 80>;

        Time next_action_timer_ = seconds(1);
        Time next_weapon_action_timer_ = seconds(1) + milliseconds(500);
        bool rescan_ = false;
        u32 weapon_update_index_ = 0;

        IdBuffer local_chrs_;
        IdBuffer boarded_chrs_;

        u32 local_buffer_index_ = 0;
        u32 boarded_buffer_index_ = 0;

        bool any_chr_moved_ = false;

        void update(Time delta);
        void update_weapon_targets(Time delta);

        void run();
    };

    DynamicMemory<AIState> ai_state_;


    Time last_key_ = 0;

    Time last_touch_held_time_ = 0;

    Time key_held_timers_[static_cast<int>(Key::count)];
};



} // namespace skyland
