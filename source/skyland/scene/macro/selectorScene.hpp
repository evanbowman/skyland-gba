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

#include "macrocosmScene.hpp"



namespace skyland::macro
{



class SelectorScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr update(Player& player, macro::EngineImpl& state) override;


    void show_island_size()
    {
        show_island_size_ = true;
    }


private:
    void describe_selected(macro::EngineImpl& state);

    Optional<Text> text_;
    Optional<Text> text_2_;
    Optional<Text> paused_text_;
    bool await_start_key_ = false;
    bool show_island_size_ = false;
    bool paused_ = false;

    Optional<Text> mv_text_;
    Optional<Text> rot_text_;
};



} // namespace skyland::macro
