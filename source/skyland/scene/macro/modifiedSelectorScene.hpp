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



class ModifiedSelectorScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr update(Player& player, macro::EngineImpl& state) override;


private:
    Optional<Text> rotate_text_;
    Optional<Text> layers_text_;
    Optional<Text> cursor_text_;
    Optional<Text> visible_layers_text_;
};



} // namespace skyland::macro
