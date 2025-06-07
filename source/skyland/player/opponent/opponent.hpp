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


#include "skyland/player/player.hpp"



class Platform;



namespace skyland
{



class App;



class Opponent : public Player
{
public:
    virtual ~Opponent()
    {
    }


    virtual bool is_friendly() const
    {
        return false;
    }


    void on_room_damaged(Room&) override;
};



} // namespace skyland
