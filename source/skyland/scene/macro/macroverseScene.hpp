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
#include "skyland/macrocosmEngine.hpp"
#include "skyland/scene.hpp"



namespace skyland::macro
{


class MacroverseScene : public Scene
{
public:
    MacroverseScene(bool fastload = false) : fastload_(fastload)
    {
    }


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;

private:
    enum class State {
        reveal,
        wait,
        fade_in,
        show,
        options,
        options_2,
        text_prompt,
        create_colony_options,
        create_colony,
        select_colony_layout,
    } state_ = State::reveal;


    void describe_selected(Platform& pfrm, macro::EngineImpl& state);


    void show_layout_text(Platform& pfrm);


    Microseconds timer_;

    Vec2<s8> selected_ = {};
    Vec2<s8> initial_sector_;
    Vec2<Float> camera_;
    std::optional<Vec2<s8>> selected_colony_;
    bool outpost_colony_;

    terrain::Sector::Shape shape_ = terrain::Sector::Shape::cube;

    Buffer<Vec2<s8>, 4> colony_create_slots_;

    bool exit_ = false;
    bool fastload_ = false;

    u8 opt_cursor_ = 0;

    Buffer<Text, 6> text_objs_;
};



} // namespace skyland::macro
