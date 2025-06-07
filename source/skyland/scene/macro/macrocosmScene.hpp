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
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"



namespace skyland::macro
{



class MacrocosmScene : public Scene
{
public:
    MacrocosmScene();


    void enter(Scene& prev) override final;
    void exit(Scene& next) override final;


    virtual void enter(macro::EngineImpl&, Scene& prev);
    virtual void exit(macro::EngineImpl&, Scene& next);


    ScenePtr update(Time delta) override final;


    void display() override final;


    MacrocosmScene* cast_macrocosm_scene() override
    {
        return this;
    }


    virtual ScenePtr update(Player& player, macro::EngineImpl& state);

    virtual void display(macro::EngineImpl& state);


    void draw_compass(macro::EngineImpl& state);
    void draw_keylock(macro::EngineImpl& state);


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


    void draw_year(macro::EngineImpl& state);

private:
    int current_season(Time year_timer, Time secs_per_season);


    Time year_length(macro::EngineImpl& state);


    struct UIObjects
    {
        Optional<UIMetric> food_;
        Optional<UIMetric> population_;
        Optional<UIMetric> productivity_;

        Optional<UIMetric> lumber_;
        Optional<UIMetric> stone_;
        Optional<UIMetric> crystal_;
        Optional<UIMetric> water_;
        Optional<UIMetric> clay_;
    };

    u8 last_season_ = 255;
    bool update_ui_on_exit_ = false;
    Optional<DynamicMemory<UIObjects>> ui_;
};



} // namespace skyland::macro
