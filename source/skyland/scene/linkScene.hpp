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
    Optional<Text> t_;

    void enter(Scene&) override
    {
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);

        PLATFORM.speaker().stream_music("unaccompanied_wind", 0);

        auto str = SYSTR(mt_hint);
        u8 mr = centered_text_margins(utf8::len(str->c_str()));
        t_.emplace(OverlayCoord{mr, 8});
        FontColors c{custom_color(0x163061), ColorConstant::silver_white};
        t_->assign(str->c_str(), c);
    }

    void exit(Scene&) override
    {
        t_.reset();
    }

    ScenePtr update(Time) override
    {
        if (key_down<Key::start>()) {
            return make_scene<MultiplayerConnectScene>();
        }
        if (key_down<Key::action_2>()) {
            return make_scene<TitleScreenScene>();
        }
        return null_scene();
    }
};



} // namespace skyland
