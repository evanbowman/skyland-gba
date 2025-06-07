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


    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void display() override;


    static bool enable_custom_scripts()
    {
        return true;
    }


private:
    void update_parameter(u8 line_num);

    u32 cursor_ = 0;

    bool unveil_ = false;

    Optional<Text> title_;
    Optional<Text> help_;

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

    Time long_hold_time_[2] = {0, 0};

    bool cancelled_ = false;

    static const ParameterInfo param_info[decltype(parameters_)::capacity()];

    static Factory factory_;
};



} // namespace skyland
