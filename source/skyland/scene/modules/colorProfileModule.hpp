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


#include "script/lisp.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class ColorProfileModule : public Module<ColorProfileModule>
{
public:
    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    static SystemString module_name()
    {
        return SystemString::module_colormode;
    }


    static u16 icon()
    {
        return 4088;
    }


    static bool run_scripts()
    {
        return false;
    }


    static StringBuffer<64> load_current_profile();


private:
    Optional<lisp::Protected> options_;
    Optional<Text> title_;
    Buffer<Text, 7> text_;
    int sel_ = 0;
    int last_sel_ = -1;

    void bind_selected_profile();

    static Factory factory_;
};



} // namespace skyland
