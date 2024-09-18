////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    void update_weapon_targets() override;


    void delay_autofire(Time duration) override;


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
