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



class TileOptionsScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr update(Player& player, macro::EngineImpl& state) override;


    void show_options(macro::EngineImpl& state);


    void msg(macro::EngineImpl& state, const char* text);



    struct OptionInfo;


private:
    void collect_options(macro::EngineImpl& state);


    Buffer<const OptionInfo*, 6> options_;

    static const OptionInfo* last_option_;
    u8 selector_ = 0;
    Optional<Text> text_;
};



} // namespace skyland::macro
