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


#include "skyland/entity.hpp"



namespace skyland
{



class Island;



class Projectile : public Entity
{
public:
    Projectile(const HitBox::Dimension& dimension) : Entity(dimension)
    {
    }


protected:
    virtual void destroy(bool explosion);


    void destroy_out_of_bounds(Island* target);
};



} // namespace skyland
