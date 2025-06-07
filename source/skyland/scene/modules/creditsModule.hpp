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


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    static bool stop_sound()
    {
        return false;
    }


    Optional<DeferredScene> next_scene_;


private:
    enum class State {
        fade_in,
        fade_out_next,
        fade_out_exit,
        page_swap,
        idle,
    } state_ = State::fade_in;


    bool load_page(u32 page);


    Buffer<Text, 12> lines_;
    Time timer_ = 0;

    int page_ = 0;

    static Factory factory_;
};



} // namespace skyland
