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


    void enter(Scene& prev) override;
    void exit(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void display() override;

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


    void describe_selected(macro::EngineImpl& state);


    void show_layout_text();


    Time timer_;

    Vec2<s8> selected_ = {};
    Vec2<s8> initial_sector_;
    Vec2<Float> camera_;
    Optional<Vec2<s8>> selected_colony_;

    terrain::Sector::Shape shape_ = terrain::Sector::Shape::cube;

    Buffer<Vec2<s8>, 4> colony_create_slots_;

    bool exit_ = false;
    bool fastload_ = false;
    bool abandon_ = false;

    u8 opt_cursor_ = 0;

    Buffer<Text, 6> text_objs_;
};



} // namespace skyland::macro
