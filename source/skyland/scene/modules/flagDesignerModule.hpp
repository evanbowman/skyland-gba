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


    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    static bool run_scripts()
    {
        return false;
    }


    u8 get_pixel(App& app, u8 x, u8 y) override;
    void set_pixel(App& app, u8 x, u8 y, u8 value) override;


    void show(Platform&, App&) override;

    bool changed_ = false;

    bool editing_ingame_ = false;

private:
    int target_y_ = 0;

    static Factory factory_;
};



} // namespace skyland
