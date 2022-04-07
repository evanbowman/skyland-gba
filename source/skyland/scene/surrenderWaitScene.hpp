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

#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SurrenderWaitScene : public WorldScene
{
public:
    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (auto next = WorldScene::update(pfrm, app, delta)) {
            return next;
        }

        if (timer_ < milliseconds(350)) {
            timer_ += delta;
            if (timer_ > milliseconds(350)) {
                app.invoke_script(pfrm, "/scripts/event/surrender.lisp");
            }
        } else {
            if (not app.dialog_buffer()) {
                return scene_pool::alloc<ReadyScene>();
            }
        }

        return null_scene();
    }


private:
    Microseconds timer_ = 0;
};



} // namespace skyland
