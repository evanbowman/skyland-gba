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


    void enter(Platform& pfrm, App& app, Scene& prev) override final;
    void exit(Platform& pfrm, App& app, Scene& next) override final;


    virtual void enter(Platform& pfrm, macro::EngineImpl&, Scene& prev);
    virtual void exit(Platform& pfrm, macro::EngineImpl&, Scene& next);


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override final;


    void display(Platform&, App&) override final;


    virtual ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state);

    virtual void display(Platform& pfrm, macro::EngineImpl& state);


    void draw_compass(Platform& pfrm, macro::EngineImpl& state);


    void drop_ui()
    {
        ui_.reset();
    }


    void update_ui(macro::EngineImpl& state);


    bool should_update_ui_after_exit() const
    {
        return update_ui_on_exit_;
    }


    void update_ui_on_exit()
    {
        update_ui_on_exit_ = true;
    }


private:
    struct UIObjects
    {
        std::optional<UIMetric> coins_;
        std::optional<UIMetric> population_;
        std::optional<UIMetric> food_;
        std::optional<UIMetric> employment_;
        std::optional<UIMetric> housing_;
        std::optional<UIMetric> happiness_;
    };

    bool update_ui_on_exit_ = false;
    std::optional<DynamicMemory<UIObjects>> ui_;
};



} // namespace skyland::macro
