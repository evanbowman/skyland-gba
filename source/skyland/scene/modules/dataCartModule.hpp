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


#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "skyland/dataCart.hpp"
#include "skyland/scene/module.hpp"



namespace skyland
{



class DataCartModule : public Module<DataCartModule>
{
public:
    bool skip_intro_;
    bool skip_dialog_ = false;

    DataCartModule(bool skip_intro = false) : skip_intro_(skip_intro)
    {
    }

    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    static SystemString module_name()
    {
        return SystemString::module_cart_viewer;
    }


    static u16 icon()
    {
        return 4056;
    }


    static bool run_scripts()
    {
        return false;
    }


    void show_cart(int index);


    ScenePtr boot_cart(int cart_index);


    void set_index(int index)
    {
        cart_index_ = index;
    }


private:
    enum class State {
        init,
        fade_in,
        wait_0,
        fade_partial,
        select,
        anim_out,
        drop,
        done,
        wait,
        booting,
        boot,
        exit,
    } state_ = State::init;

    int cart_index_ = 0;
    int timer_ = 0;
    Float cart_scroll_down_ = 0;

    Optional<DataCartLibrary> carts_;

    Time wait_time_ = 0;

    static Factory factory_;
};



} // namespace skyland
