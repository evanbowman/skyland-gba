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

#include "worldScene.hpp"



namespace skyland
{



class SwapOverlayTextureScene : public ActiveWorldScene
{
public:
    SwapOverlayTextureScene(const char* texture_name, DeferredScene next_scene)
        : next_scene_(next_scene), next_texture_(texture_name)
    {
    }


    ScenePtr update(Time delta) override
    {
        auto next = ActiveWorldScene::update(delta);

        PLATFORM.load_overlay_texture(next_texture_);

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
