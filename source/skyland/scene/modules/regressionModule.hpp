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



class RegressionModule : public Module<RegressionModule>
{
public:
    ScenePtr update(Time delta) override;


    static SystemString module_name()
    {
        return SystemString::module_regression;
    }


    static u16 icon()
    {
        return 4072;
    }


    static bool run_scripts()
    {
        return true;
    }


private:
    static Factory factory_;
};



} // namespace skyland
