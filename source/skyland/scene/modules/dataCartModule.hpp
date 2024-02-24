////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> update(Time delta) override;


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


    ScenePtr<Scene> boot_cart(int cart_index);


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
