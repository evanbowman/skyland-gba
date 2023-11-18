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
#include "multiplayerConnectScene.hpp"
#include "skyland/scene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/systemString.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



class LinkScene : public Scene
{
public:
    std::optional<Text> t_;

    void enter(App&, Scene&) override
    {
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);

        PLATFORM.speaker().play_music("unaccompanied_wind", 0);

        auto str = SYSTR(mt_hint);
        u8 mr = centered_text_margins(utf8::len(str->c_str()));
        t_.emplace(OverlayCoord{mr, 8});
        FontColors c{custom_color(0x163061), ColorConstant::silver_white};
        t_->assign(str->c_str(), c);
    }

    void exit(App&, Scene&) override
    {
        t_.reset();
    }

    ScenePtr<Scene> update(App&, Microseconds) override
    {
        if (key_down<Key::start>()) {
            return scene_pool::alloc<MultiplayerConnectScene>();
        }
        if (key_down<Key::action_2>()) {
            return scene_pool::alloc<TitleScreenScene>();
        }
        return null_scene();
    }
};



} // namespace skyland
