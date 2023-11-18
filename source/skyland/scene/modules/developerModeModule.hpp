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


    void enter(App& app, Scene& prev) override;
    void exit(App& app, Scene& next) override;


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


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void set_opt(bool value);


    static Factory factory_;

    std::optional<TextView> message_;
    std::optional<TextView> message_overflow_;
    std::optional<Text> option_text_;
    bool was_developer_mode_ = false;

    bool exit_ = false;

    bool option_ = false;
};



} // namespace skyland
