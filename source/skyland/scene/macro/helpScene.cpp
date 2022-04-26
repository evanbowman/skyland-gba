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


#include "helpScene.hpp"
#include "macroverseScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void HelpScene::enter(Platform& pfrm, App&, Scene&)
{
    pfrm.screen().schedule_fade(1.f);
}



void HelpScene::exit(Platform& pfrm, App&, Scene&)
{
}



ScenePtr<Scene> HelpScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (player(app).key_down(pfrm, Key::action_1) or
        player(app).key_down(pfrm, Key::action_2)) {
        pfrm.load_overlay_texture("overlay_challenges");

        pfrm.speaker().play_music(app.environment().music(), 0);


        return scene_pool::alloc<MacroverseScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
