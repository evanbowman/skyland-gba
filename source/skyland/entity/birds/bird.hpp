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

#include "skyland/coord.hpp"
#include "skyland/entity.hpp"



namespace skyland
{



class Island;



class Bird : public Entity
{
public:
    using Entity::Entity;


    virtual ~Bird()
    {
    }


    virtual void signal()
    {
    }


    virtual Island* island()
    {
        return nullptr;
    }


    virtual RoomCoord coordinate()
    {
        return {};
    }


    virtual u8 icon() const
    {
        return 19;
    }
};



} // namespace skyland
