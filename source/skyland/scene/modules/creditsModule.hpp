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



class CreditsModule : public Module<CreditsModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_credits;
    }


    static u16 icon()
    {
        return 1752;
    }


    static bool run_scripts()
    {
        return false;
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    static bool stop_sound()
    {
        return false;
    }


    bool autoadvance_ = false;


    std::optional<DeferredScene> next_scene_;


private:
    enum class State {
        fade_in,
        fade_out_next,
        fade_out_exit,
        page_swap,
        idle,
    } state_ = State::fade_in;


    void load_page(Platform& pfrm, u32 page);


    Buffer<Text, 12> lines_;
    Microseconds timer_ = 0;

    int page_ = 0;

    static Factory factory_;
};



} // namespace skyland
