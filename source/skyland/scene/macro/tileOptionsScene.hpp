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


#include "macrocosmScene.hpp"



namespace skyland::macro
{



class TileOptionsScene : public MacrocosmScene
{
public:
    void enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev) override;


    void exit(Platform& pfrm, macro::EngineImpl& state, Scene& next) override;


    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state) override;


    void show_options(Platform& pfrm, macro::EngineImpl& state);


    void msg(Platform& pfrm, macro::EngineImpl& state, const char* text);



    struct OptionInfo;


private:
    void collect_options(Platform& pfrm, macro::EngineImpl& state);


    Buffer<const OptionInfo*, 6> options_;

    static const OptionInfo* last_option_;
    u8 selector_ = 0;
    std::optional<Text> text_;
};



} // namespace skyland::macro
