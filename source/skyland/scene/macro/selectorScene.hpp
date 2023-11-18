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



class SelectorScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override;


    void exit(macro::EngineImpl& state, Scene& next) override;


    ScenePtr<Scene> update(Player& player, macro::EngineImpl& state) override;


    void show_island_size()
    {
        show_island_size_ = true;
    }


private:
    void describe_selected(macro::EngineImpl& state);

    std::optional<Text> text_;
    std::optional<Text> text_2_;
    std::optional<Text> paused_text_;
    bool await_start_key_ = false;
    bool show_island_size_ = false;
    bool paused_ = false;

    std::optional<Text> mv_text_;
    std::optional<Text> rot_text_;
};



} // namespace skyland::macro
