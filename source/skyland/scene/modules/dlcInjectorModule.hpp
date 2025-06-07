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


#include "skyland/scene/module.hpp"



namespace skyland
{



class DlcInjectorModule : public Module<DlcInjectorModule>
{
public:
    DlcInjectorModule(bool begin_load = false) : begin_load_(begin_load)
    {
    }


    static SystemString module_name()
    {
        return SystemString::module_update_loader;
    }


    static u16 icon()
    {
        return 1192;
    }


    static bool run_scripts()
    {
        return false;
    }


    ScenePtr update(Time delta) override;


    bool begin_load_ = false;


    static Factory factory_;
};



} // namespace skyland
