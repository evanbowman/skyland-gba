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


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class FactoryResetModule : public Module<FactoryResetModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_factory_reset;
    }


    static u16 icon()
    {
        return 968;
    }


    static bool run_scripts()
    {
        return false;
    }


    ScenePtr update(Time delta) override;


    Optional<TextView> text_;
    int key_count_ = 0;

    static Factory factory_;
};



} // namespace skyland
