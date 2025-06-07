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

#include "platform/platform.hpp"
#include "player.hpp"
#include "script/lisp.hpp"



namespace skyland
{



class AutopilotPlayer : public Player
{
public:
    AutopilotPlayer(lisp::Value* keys_list);


    void update(Time delta) override;


    bool key_down(Key k) override;


    bool key_up(Key k) override;


    bool key_pressed(Key k) override;


    void on_room_damaged(Room& room) override;


private:
    lisp::Protected keys_list_;

    Time next_key_timeout_ = 0;
    Optional<Key> next_timeout_key_;
    bool next_timeout_release_ = false;


    Time key_tap_timeout_ = 0;


    Platform::Keyboard::KeyStates prev_;
    Platform::Keyboard::KeyStates states_;

    Platform::Keyboard::KeyStates taps_;
};



} // namespace skyland
