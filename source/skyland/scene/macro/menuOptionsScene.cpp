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



#include "menuOptionsScene.hpp"
#include "macroverseScene.hpp"
#include "nextTurnScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void MenuOptionsScene::enter(Platform& pfrm,
                             macro::EngineImpl& state,
                             Scene& prev)
{
    if (auto m = prev.cast_macrocosm_scene()) {
        m->drop_ui();
    }

    pfrm.set_tile(Layer::overlay, 1, 1, 393);
    pfrm.set_tile(Layer::overlay, 1, 2, 394);

    StringBuffer<32> mv(":");
    mv += SYSTR(start_menu_macroverse)->c_str();

    macroverse_text_.emplace(pfrm, mv.c_str(), OverlayCoord{2, 1});
    next_turn_text_.emplace(
        pfrm, SYSTR(macro_next_turn)->c_str(), OverlayCoord{2, 2});
}



void MenuOptionsScene::exit(Platform& pfrm,
                            macro::EngineImpl& state,
                            Scene& next)
{

    MacrocosmScene::exit(pfrm, state, next);

    macroverse_text_.reset();

    pfrm.set_tile(Layer::overlay, 1, 1, 0);
}



ScenePtr<Scene> MenuOptionsScene::update(Platform& pfrm,
                                         Player& player,
                                         macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    ++frames_;

    if (exit_timer_) {
        // Show the menu for at least a few frames, as an indication to players
        // that they need to hold down the button.
        ++exit_timer_;
        if (exit_timer_ > 6) {
            return scene_pool::alloc<SelectorScene>();
        } else {
            return null_scene();
        }
    }

    if (player.key_pressed(pfrm, Key::alt_1) or
        player.key_pressed(pfrm, Key::alt_2)) {

        // if (player.key_down(pfrm, Key::action_1)) {
        //     pfrm.speaker().play_sound("cursor_tick", 0);
        //     return scene_pool::alloc<CitizensInfoScene>();
        // } else if (player.key_down(pfrm, Key::left)) {
        //     pfrm.speaker().play_sound("cursor_tick", 0);
        //     return scene_pool::alloc<ViewBudgetScene>();
        if (player.key_down(pfrm, Key::right)) {
            pfrm.speaker().play_sound("cursor_tick", 0);
            return scene_pool::alloc<NextTurnScene>();
        } // else if (player.key_down(pfrm, Key::down)) {
        //     pfrm.speaker().play_sound("cursor_tick", 0);
        //     return scene_pool::alloc<ViewCommoditiesScene>();
        // } else
        if (player.key_down(pfrm, Key::up)) {
            pfrm.speaker().play_sound("cursor_tick", 0);
            pfrm.fill_overlay(0);
            return scene_pool::alloc<MacroverseScene>(true);
        }

    } else {
        if (frames_ > 15) {
            return scene_pool::alloc<SelectorScene>();
        } else {
            exit_timer_ = 1;
        }
    }

    return null_scene();
}



} // namespace skyland::macro
