////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "fullscreenDialogChainScene.hpp"


namespace skyland
{



ScenePtr process_script_menu_request();



void FullscreenDialogChainScene::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(0);
    PLATFORM.screen().schedule_fade(1);
}



ScenePtr FullscreenDialogChainScene::update(Time delta)
{
    if (auto scn = process_script_menu_request()) {
        return scn;
    }

    if (APP.dialog_buffer()) {
        auto buffer = std::move(*APP.dialog_buffer());
        APP.dialog_buffer().reset();
        auto scn = make_scene<BoxedDialogScene>(std::move(buffer));
        scn->show_coins_ = false;
        scn->set_next_scene(make_deferred_scene<FullscreenDialogChainScene>());
        return scn;
    }
    return null_scene();
}



}
