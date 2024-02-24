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
