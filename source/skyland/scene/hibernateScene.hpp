////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "graphics/overlay.hpp"
#include "platform/platform.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "startMenuScene.hpp"



namespace skyland
{



class HibernateScene : public Scene
{
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override
    {
        text_.emplace(pfrm);
        text_->assign(SYSTR(misc_hibernate_message)->c_str(), {1, 4}, {28, 8});
    }


    void exit(Platform& pfrm, App& app, Scene& next) override
    {
        pfrm.fill_overlay(0);
    }


    ScenePtr<Scene>
    update(Platform& pfrm, App& app, Microseconds delta) override
    {
        if (key_down<Key::action_1>(pfrm)) {
            text_.reset();
            pfrm.screen().display();
            info(pfrm, "enter hibernate...");
            pfrm.system_call("hibernate", nullptr);
            info(pfrm, "resume!");
            return scene_pool::alloc<StartMenuScene>(1);
        }

        return null_scene();
    }


private:
    std::optional<TextView> text_;
};



} // namespace skyland
