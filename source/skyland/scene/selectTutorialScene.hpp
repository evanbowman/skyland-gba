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
#include "module.hpp"
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class SelectTutorialScene : public Module<SelectTutorialScene>
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    // The scene will load the tutorial and return immediately.
    void quick_select(int tutorial_number);


    static int tutorial_count();


    ScenePtr update(Time delta) override;


    void display() override;


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
    void show_options();

    enum class State {
        fade_in,
        idle,
        fade_out,
        quickselect,
    } state_ = State::idle;


    Optional<lisp::Protected> tutorials_;
    Buffer<Text, 5> text_;

    int page_ = 0;
    int cursor_ = 0;

    int page_count_ = 0;

    Time timer_ = 0;

    bool exit_ = false;


    static Factory factory_;
};



} // namespace skyland
