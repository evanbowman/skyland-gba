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


#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class MacrocosmFreebuildModule : public Module<MacrocosmFreebuildModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_macro;
    }


    static u16 icon()
    {
        return 2552;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void init(Platform&, App&);


    static Factory factory_;

    float scroll_ = 0;

    Vec3<u8> cursor_;
};



} // namespace skyland