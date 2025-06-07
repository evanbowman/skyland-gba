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



class MenuOptionsScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr update(Player& player, macro::EngineImpl& state) override;


private:
    Optional<Text> next_turn_text_;
    Optional<Text> macroverse_text_;
    Optional<Text> harvest_text_;
    Optional<Text> message_text_;
    int exit_timer_ = 0;
    u32 frames_ = 0;
};



} // namespace skyland::macro
