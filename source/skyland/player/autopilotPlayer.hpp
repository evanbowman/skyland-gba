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
    AutopilotPlayer(lisp::Value* buttons_list);


    void update(Time delta) override;


    bool button_down(Button k) override;


    bool button_up(Button k) override;


    bool button_pressed(Button k) override;


    void on_room_damaged(Room& room) override;


private:
    lisp::Protected buttons_list_;

    Time next_button_timeout_ = 0;
    Optional<Button> next_timeout_button_;
    bool next_timeout_release_ = false;


    Time button_tap_timeout_ = 0;


    Platform::Input::ButtonStates prev_;
    Platform::Input::ButtonStates states_;

    Platform::Input::ButtonStates taps_;
};



} // namespace skyland
