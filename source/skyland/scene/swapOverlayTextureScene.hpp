#pragma once

#include "worldScene.hpp"



namespace skyland {



class SwapOverlayTextureScene : public ActiveWorldScene
{
public:
    SwapOverlayTextureScene(const char* texture_name, DeferredScene next_scene)
        : next_scene_(next_scene), next_texture_(texture_name)
    {
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        auto next = ActiveWorldScene::update(pfrm, app, delta);

        pfrm.load_overlay_texture(next_texture_);

        if (next) {
            return next;
        } else {
            return next_scene_();
        }
    }



private:
    DeferredScene next_scene_;
    const char* next_texture_;
};



} // namespace skyland
