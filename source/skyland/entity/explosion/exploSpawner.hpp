////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/entity.hpp"



namespace skyland
{



class ExploSpawner : public Entity
{
public:
    ExploSpawner(const Vec2<Fixnum>& pos) : Entity({})
    {
        sprite_.set_position(pos);
        sprite_.set_alpha(Sprite::Alpha::transparent);
    }


    void update(Microseconds delta) override;


    void rewind(Microseconds delta) override
    {
        kill();
    }


    static void create(const Vec2<Fixnum>& pos);


private:
    Microseconds timer1_ = 0;
    Microseconds timer2_ = 0;
};



} // namespace skyland
