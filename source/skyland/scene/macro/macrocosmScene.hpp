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
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"



namespace skyland::macro
{



class MacrocosmScene : public Scene
{
public:
    MacrocosmScene();


    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override final;


    void display(Platform&, App&) override final;


    virtual ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::State& state);

    virtual void display(Platform& pfrm, macro::State& state);


    void draw_compass(Platform& pfrm, macro::State& state);


private:
    struct UIObjects
    {
        std::optional<UIMetric> coins_;
        std::optional<UIMetric> population_;
        std::optional<UIMetric> food_;
        std::optional<UIMetric> employment_;
        std::optional<UIMetric> housing_;
    };

    DynamicMemory<UIObjects> ui_;
};



} // namespace skyland::macro
