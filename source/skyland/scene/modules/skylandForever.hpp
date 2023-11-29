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


    ScenePtr<Scene> update(Microseconds delta) override;


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

    std::optional<Text> title_;
    std::optional<Text> help_;

    std::optional<TextView> msg_;

    static Factory factory_;
};



} // namespace skyland
