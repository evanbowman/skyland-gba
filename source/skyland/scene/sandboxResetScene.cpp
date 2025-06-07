////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sandboxResetScene.hpp"
#include "modules/sandboxLoaderModule.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



static const auto sel_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};



ScenePtr SandboxResetScene::update(Time delta)
{
    if (APP.player().key_down(Key::up)) {
        selection_ = true;
        yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
        no_text_->assign(SYSTR(exit)->c_str());
    }

    if (APP.player().key_down(Key::down)) {
        selection_ = false;
        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(exit)->c_str(), sel_colors);
    }

    if (APP.player().key_down(Key::action_1)) {
        if (selection_) {
            return make_scene<SandboxLoaderModule>();
        } else {
            return make_scene<TitleScreenScene>(3);
        }
    }

    return null_scene();
}


void SandboxResetScene::enter(Scene& prev)
{
    msg_.emplace(SYSTR(reset_sandbox_query)->c_str(), OverlayCoord{1, 1});
    yes_text_.emplace(OverlayCoord{2, 3});
    no_text_.emplace(SYSTR(exit)->c_str(), OverlayCoord{2, 5});

    yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
}



void SandboxResetScene::exit(Scene& next)
{
    msg_.reset();
    yes_text_.reset();
    no_text_.reset();
}



} // namespace skyland
