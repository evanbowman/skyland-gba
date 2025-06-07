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
                return make_scene<ReadyScene>();
            }
        }

        return null_scene();
    }


private:
    Time timer_ = 0;
};



} // namespace skyland
