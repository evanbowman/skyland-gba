////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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


    ScenePtr<Scene> update(Time delta) override final;


    void display() override final;


    MacrocosmScene* cast_macrocosm_scene() override
    {
        return this;
    }


    virtual ScenePtr<Scene> update(Player& player, macro::EngineImpl& state);

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
