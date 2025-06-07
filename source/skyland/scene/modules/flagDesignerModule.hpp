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


#include "skyland/flag.hpp"
#include "skyland/paint.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class FlagDesignerModule : public Module<FlagDesignerModule>, public Paint
{

public:
    static SystemString module_name()
    {
        return SystemString::module_flag_designer;
    }


    static u16 icon()
    {
        return 952;
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    static bool run_scripts()
    {
        return false;
    }


    u8 get_pixel(u8 x, u8 y) override;
    void set_pixel(u8 x, u8 y, u8 value) override;


    void show() override;

    bool changed_ = false;

    bool editing_ingame_ = false;

private:
    int target_y_ = 0;

    static Factory factory_;
};



} // namespace skyland
