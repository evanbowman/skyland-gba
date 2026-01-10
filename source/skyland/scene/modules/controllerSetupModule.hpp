////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene/module.hpp"
#include "skyland/settings.hpp"



namespace skyland
{



class ControllerSetupModuleInit  : public Module<ControllerSetupModuleInit>
{
public:

    static SystemString module_name()
    {
        return SystemString::module_button_mapping;
    }


    static u16 icon()
    {
        return 2472;
    }


    static bool run_scripts()
    {
        return false;
    }


    static bool stop_sound()
    {
        return false;
    }


    ScenePtr update(Time delta) override;


    static Factory factory_;

};



class ControllerSetupModule : public Scene
{
public:
    ControllerSetupModule()
    {
    }


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void repaint();


    Optional<Function<8, void(lisp::Value*)>> on_select_;

    settings::Settings settings_;
    Optional<TextView> message_;
    Optional<TextView> message_overflow_;
    Optional<Text> option_text_;
    bool was_developer_mode_ = false;
    int key_index_ = -1;

    Vector<StringBuffer<48>> used_scancodes_;

    Optional<DeferredScene> next_;

    bool exit_ = false;

    bool option_ = false;
};



} // namespace skyland
