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
#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class MacrocosmLoaderModule : public Module<MacrocosmLoaderModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::empty;
    }


    static u16 icon()
    {
        return 2552;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Scene& prev) override;


    ScenePtr update(Time delta) override;


    static Factory factory_;

    float scroll_ = 0;

    Vec3<u8> cursor_;

    Optional<Text> loading_text_;
    bool skip_ = true;
};



} // namespace skyland
