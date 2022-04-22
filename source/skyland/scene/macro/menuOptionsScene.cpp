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
#include "nextTurnScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void MenuOptionsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    Text::platform_retain_alphabet(pfrm);
    MacrocosmScene::enter(pfrm, app, prev);

    // rotate_text_.emplace(pfrm,
    //                      SYSTR(macro_rotate)->c_str(),
    //                      OverlayCoord{3, 1});

    // layers_text_.emplace(pfrm,
    //                      SYSTR(macro_layers)->c_str(),
    //                      OverlayCoord{3, 2});

    pfrm.set_tile(Layer::overlay, 0, 19, 394);
    // pfrm.set_tile(Layer::overlay, 2, 1, 395);
    // pfrm.set_tile(Layer::overlay, 1, 2, 392);
    // pfrm.set_tile(Layer::overlay, 2, 2, 393);

    next_turn_text_.emplace(
        pfrm,
        SYSTR(macro_next_turn)->c_str(),
        OverlayCoord{1, (u8)(calc_screen_tiles(pfrm).y - 1)});

    // visible_layers_text_->assign(SYSTR(macro_visible_layers)->c_str());
    // visible_layers_text_->append(app.macrocosm()->sector().get_z_view());
}



void MenuOptionsScene::exit(Platform& pfrm, App& app, Scene& next)
{
    if (not dynamic_cast<NextTurnScene*>(&next)) {
        MacrocosmScene::exit(pfrm, app, next);
        next_turn_text_.reset();
        pfrm.set_tile(Layer::overlay, 0, 19, 0);
    } else {
        next_turn_text_->__detach();
    }
}



ScenePtr<Scene>
MenuOptionsScene::update(Platform& pfrm, Player& player, macro::State& state)
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

        if (player.key_down(pfrm, Key::left)) {

            pfrm.speaker().play_sound("cursor_tick", 2);
        } else if (player.key_down(pfrm, Key::right)) {
            pfrm.speaker().play_sound("cursor_tick", 2);
            return scene_pool::alloc<NextTurnScene>();
        } else if (player.key_down(pfrm, Key::down)) {

            pfrm.speaker().play_sound("cursor_tick", 2);
        } else if (player.key_down(pfrm, Key::up)) {

            pfrm.speaker().play_sound("cursor_tick", 2);
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
