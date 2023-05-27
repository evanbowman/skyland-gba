////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class AdventureLogScene : public Scene
{
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;

    lisp::Value* load_logentry(int entry);
    int logentry_count();

    StringBuffer<128> format_logentry(Platform& pfrm, int entry);



    void set_next_scene(DeferredScene next)
    {
        next_ = next;
    }


    void show_page(Platform&, App&, int page_num);


private:
    std::optional<DeferredScene> next_;

    enum class State {
        ready,
        page_turn_right_anim,
        page_turn_left_anim,
        page_fade_in_anim,
        fade_out,
    } state_ = State::ready;

    int page_ = 0;
    Microseconds timer_ = 0;

    bool logbook_missing_ = false;

    int max_pages_ = 0;

    Buffer<TextView, 3> entries_;
};



} // namespace skyland
