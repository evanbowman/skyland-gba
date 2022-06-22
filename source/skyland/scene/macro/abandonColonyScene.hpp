#pragma once

#include "macrocosmScene.hpp"
#include "macroverseScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/scene/boxedDialogScene.hpp"



namespace skyland::macro
{



class AbandonColonyScene : public MacrocosmScene
{
public:

    void enter(Platform& pfrm, macro::EngineImpl&, Scene& prev)
    {
        drop_ui();
    }



    ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state) override
    {
        if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
            return scene;
        }

        counter_ += 1;

        if (counter_ > 70) {

            auto buffer = allocate_dynamic<DialogString>("dialog-buffer");
            *buffer = SYSTR(grav_collapse_ended)->c_str();
            auto next =
                scene_pool::alloc<BoxedDialogScene>(std::move(buffer), false);
            next->set_next_scene([&pfrm, &state] {
                                     pfrm.speaker().play_sound("cursor_tick", 0);
                                     pfrm.fill_overlay(0);


                                     auto& current = state.sector();
                                     state.bind_sector({0, 0});
                                     state.erase_sector(current.coordinate());

                                     pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);

                                     return scene_pool::alloc<MacroverseScene>(true);
                                 });

            return next;

        }

        return null_scene();
    }

private:

    int counter_ = 0;
};



}
