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

#include "readyScene.hpp"
#include "inspectP2Scene.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SurrenderWaitScene : public WorldScene
{
public:
    ScenePtr update(Time delta) override
    {
        if (auto next = WorldScene::update(delta)) {
            return next;
        }

        if (timer_ < milliseconds(350)) {
            timer_ += delta;
            if (timer_ > milliseconds(350)) {
                APP.invoke_script("/scripts/event/surrender.lisp");
            }
        } else {
            if (not APP.dialog_buffer()) {
                if (cached_camera_far_) {
                    return make_scene<InspectP2Scene>();
                } else {
                    return make_scene<ReadyScene>();
                }
            }
        }

        return null_scene();
    }


    void enter(Scene& prev) override
    {
        if (auto ws = prev.cast_world_scene()) {
            cached_camera_far_ = ws->is_far_camera();
        }
    }


private:
    Time timer_ = 0;
    bool cached_camera_far_ = false;
};



} // namespace skyland
