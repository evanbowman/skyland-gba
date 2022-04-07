////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    void update(Platform&, App&, Microseconds delta) override;


    bool key_down(Platform&, Key k) override;


    bool key_up(Platform&, Key k) override;


    bool key_pressed(Platform&, Key k) override;


    void on_room_damaged(Platform& pfrm, App& app, Room& room) override;


private:
    lisp::Protected keys_list_;

    Microseconds next_key_timeout_ = 0;
    std::optional<Key> next_timeout_key_;
    bool next_timeout_release_ = false;


    Microseconds key_tap_timeout_ = 0;


    Platform::Keyboard::KeyStates prev_;
    Platform::Keyboard::KeyStates states_;

    Platform::Keyboard::KeyStates taps_;
};



} // namespace skyland
