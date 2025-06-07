////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "projectile.hpp"
#include "skyland/rooms/swerveMissileSilo.hpp"



namespace skyland
{



class SwerveMissile : public Projectile
{
public:
    SwerveMissile(RoomCoord origin, const SwerveMissileSilo::PathArray& path);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void on_collision(Room&, Vec2<u8>) override;


    void on_collision(Entity& entity) override;


private:
    void on_destroy();

    Time timer_ = 0;
    RoomCoord origin_;
    SwerveMissileSilo::PathArray path_;
    u8 path_index_ = 0;
    u8 flame_counter_ = 0;
};



} // namespace skyland
