#pragma once

#include "macrocosmScene.hpp"
#include "macroverseScene.hpp"
#include "skyland/scene/boxedDialogScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland::macro
{



class AbandonColonyScene : public MacrocosmScene
{
public:
    void enter(macro::EngineImpl&, Scene& prev) override
    {
    }



    ScenePtr update(Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(player, state)) {
            return scene;
        }

        if (counter_ < 10) {
            PLATFORM.screen().schedule_fade(
                counter_ / 10.f,
                {ColorConstant::rich_black, false, false, true, false});
        } else if (counter_ == 10) {
            PLATFORM.speaker().set_music_volume(2);
        }

        counter_ += 1;

        if (counter_ > 70) {

            auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
            *buffer = SYSTR(grav_collapse_ended)->c_str();
            auto next = make_scene<BoxedDialogScene>(std::move(buffer));
            next->set_next_scene([&state] {
                PLATFORM.speaker().play_sound("cursor_tick", 0);
                PLATFORM.fill_overlay(0);


                auto& current = state.sector();
                state.bind_sector({0, 0});
                state.erase_sector(current.coordinate());

                PLATFORM.speaker().set_music_volume(
                    Platform::Speaker::music_volume_max);

                return make_scene<MacroverseScene>(true);
            });

            return next;
        }

        return null_scene();
    }

private:
    int counter_ = 0;
};



} // namespace skyland::macro
