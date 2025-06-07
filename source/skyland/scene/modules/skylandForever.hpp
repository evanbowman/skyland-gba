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
#include "number/random.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class SkylandForever : public Module<SkylandForever>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_skyland_forever;
    }


    static u16 icon()
    {
        return 1736;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void display() override;


    static bool enable_custom_scripts()
    {
        return true;
    }


    static void init(u8 difficulty, rng::LinearGenerator seed);


private:
    void update_parameter(u8 line_num);

    struct ParameterInfo
    {
        SystemString name_;
        int increment_;
        int lower_limit_;
        int upper_limit_;
    };

    Buffer<Text, 2> settings_text_;
    using ParamBuffer = Buffer<int, decltype(settings_text_)::capacity()>;
    ParamBuffer parameters_;

    static const ParameterInfo param_info[decltype(parameters_)::capacity()];
    bool unveil_ = false;
    u8 cursor_ = 0;

    Optional<Text> title_;
    Optional<Text> help_;

    Optional<TextView> msg_;

    static Factory factory_;
};



} // namespace skyland
