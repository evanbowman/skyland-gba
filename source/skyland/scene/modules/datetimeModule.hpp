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

#include "dateTime.hpp"
#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class DatetimeModule : public Module<DatetimeModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_datetime;
    }


    static u16 icon()
    {
        return 4072;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool enable_custom_scripts()
    {
        return false;
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void repaint();


    Optional<DeferredScene> next_scene_;


    static bool requires_developer_mode()
    {
        return true;
    }


private:
    DateTime dt_;


    enum class State {
        set_month,
        set_year,
        set_day,
        set_hour,
        set_min,
        set_sec
    } state_ = State::set_month;


    static Factory factory_;
};



} // namespace skyland
