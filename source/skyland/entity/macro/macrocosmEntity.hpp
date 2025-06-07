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



namespace skyland::macro
{


struct Engine;



class MacrocosmEntity : public Entity
{
public:
    MacrocosmEntity() : Entity({})
    {
        health_ = 1;
    }


    virtual void update(macro::Engine& s, Time delta)
    {
    }


    void update(Time delta) override final;


private:
    // ...
};



} // namespace skyland::macro
