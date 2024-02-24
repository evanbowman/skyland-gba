////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
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

        PLATFORM.speaker().play_music("unaccompanied_wind", 0);

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

    ScenePtr<Scene> update(Time) override
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
