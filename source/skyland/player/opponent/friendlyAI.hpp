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


#include "opponent.hpp"



namespace skyland
{



class Cannon;
class MissileSilo;



class FriendlyAI : public Opponent
{
public:
    void update(Time delta) override;


    bool is_friendly() const override
    {
        return true;
    }


    void on_room_damaged(Room&) override;


    void on_level_start() override;
};



} // namespace skyland
