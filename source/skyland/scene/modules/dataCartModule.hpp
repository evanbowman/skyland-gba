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

    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


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


    void show_cart(Platform& pfrm, int index);


    ScenePtr<Scene> boot_cart(Platform& pfrm, int cart_index);


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

    std::optional<DataCartLibrary> carts_;

    Microseconds wait_time_ = 0;

    static Factory factory_;
};



} // namespace skyland
