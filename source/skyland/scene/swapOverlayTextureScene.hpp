////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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
