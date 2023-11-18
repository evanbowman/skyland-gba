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
#include "module.hpp"
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class SelectTutorialScene : public Module<SelectTutorialScene>
{
public:
    void enter(App&, Scene& prev) override;
    void exit(App&, Scene& next) override;


    // The scene will load the tutorial and return immediately.
    void quick_select(int tutorial_number);


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


    static bool run_scripts()
    {
        return true;
    }


    static SystemString module_name()
    {
        return SystemString::module_tutorials;
    }


    static u16 icon()
    {
        return 984;
    }



private:
    void show_options(App&);

    enum class State {
        fade_in,
        idle,
        fade_out,
        quickselect,
    } state_ = State::idle;


    std::optional<lisp::Protected> tutorials_;
    Buffer<Text, 5> text_;

    int page_ = 0;
    int cursor_ = 0;

    int page_count_ = 0;

    Microseconds timer_ = 0;

    bool exit_ = false;


    static Factory factory_;
};



} // namespace skyland
