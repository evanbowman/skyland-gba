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


#include "sandboxResetScene.hpp"
#include "modules/sandboxLoaderModule.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



static const auto sel_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};



ScenePtr<Scene>
SandboxResetScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::up)) {
        selection_ = true;
        yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
        no_text_->assign(SYSTR(exit)->c_str());
    }

    if (app.player().key_down(pfrm, Key::down)) {
        selection_ = false;
        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(exit)->c_str(), sel_colors);
    }

    if (app.player().key_down(pfrm, Key::action_1)) {
        if (selection_) {
            return scene_pool::alloc<SandboxLoaderModule>();
        } else {
            return scene_pool::alloc<TitleScreenScene>(3);
        }
    }

    return null_scene();
}


void SandboxResetScene::enter(Platform& pfrm, App&, Scene& prev)
{
    msg_.emplace(pfrm, SYSTR(reset_sandbox_query)->c_str(), OverlayCoord{1, 1});
    yes_text_.emplace(pfrm, OverlayCoord{2, 3});
    no_text_.emplace(pfrm, SYSTR(exit)->c_str(), OverlayCoord{2, 5});

    yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
}



void SandboxResetScene::exit(Platform&, App&, Scene& next)
{
    msg_.reset();
    yes_text_.reset();
    no_text_.reset();
}



} // namespace skyland
