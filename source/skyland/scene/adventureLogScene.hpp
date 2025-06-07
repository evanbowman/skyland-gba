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
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class AdventureLogScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;

    lisp::Value* load_logentry(int entry);
    int logentry_count();

    StringBuffer<128> format_logentry(int entry);



    void set_next_scene(DeferredScene next)
    {
        next_ = next;
    }


    void show_page(int page_num);


private:
    Optional<DeferredScene> next_;

    enum class State {
        ready,
        page_turn_right_anim,
        page_turn_left_anim,
        page_fade_in_anim,
        fade_out,
    } state_ = State::ready;

    int page_ = 0;
    Time timer_ = 0;

    bool logbook_missing_ = false;

    int max_pages_ = 0;

    Buffer<TextView, 3> entries_;
};



} // namespace skyland
