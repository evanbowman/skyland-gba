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
        return 1576;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool enable_custom_scripts()
    {
        return false;
    }


    void enter(App& app, Scene& prev) override;
    void exit(App& app, Scene& next) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void repaint();


    std::optional<DeferredScene> next_scene_;


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
