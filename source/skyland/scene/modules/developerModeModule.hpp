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



class DeveloperModeModule : public Module<DeveloperModeModule>
{
public:
    DeveloperModeModule()
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    static SystemString module_name()
    {
        return SystemString::module_developer_mode;
    }


    static u16 icon()
    {
        return 2472;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool stop_sound()
    {
        return false;
    }


    ScenePtr update(Time delta) override;


    void set_opt(bool value);


    static Factory factory_;

    Optional<TextView> message_;
    Optional<TextView> message_overflow_;
    Optional<Text> option_text_;
    bool was_developer_mode_ = false;

    bool exit_ = false;

    bool option_ = false;
};



} // namespace skyland
