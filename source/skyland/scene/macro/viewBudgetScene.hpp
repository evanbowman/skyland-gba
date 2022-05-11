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

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "macrocosmScene.hpp"
#include "skyland/macrocosmEngine.hpp"



namespace skyland::macro
{



class ViewBudgetScene : public Scene
{
public:
    ViewBudgetScene();

    void enter(Platform& pfrm, App& app, Scene& prev) override;


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override;


private:
    void show(Platform&, macro::StateImpl&);


    struct State
    {
        std::optional<Text> heading_;
        Buffer<Text, 7> lines_;
        int page_ = 0;
        int pages_ = 0;
    };

    std::optional<Text> heading_;

    DynamicMemory<State> s_;
    bool exit_ = false;
};



} // namespace skyland::macro
