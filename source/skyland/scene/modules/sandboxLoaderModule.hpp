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



class SandboxLoaderModule : public Module<SandboxLoaderModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_sandbox;
    }


    static u16 icon()
    {
        return 1176;
    }


    static bool run_scripts()
    {
        return true;
    }


    static int get_setting(u8 slot);


    void enter(App&, Scene& prev) override;


    void exit(App&, Scene& prev) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


    static bool enable_custom_scripts()
    {
        return true;
    }


private:
    void update_parameter(u8 line_num);

    u32 cursor_ = 0;

    bool unveil_ = false;

    std::optional<Text> title_;
    std::optional<Text> help_;

    struct ParameterInfo
    {
        SystemString name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Buffer<Text, 6> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    static ParamBuffer parameters_;

    Microseconds long_hold_time_[2] = {0, 0};

    bool cancelled_ = false;

    static const ParameterInfo param_info[decltype(parameters_)::capacity()];

    static Factory factory_;
};



} // namespace skyland
