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



#include "modifiedSelectorScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void ModifiedSelectorScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (auto m = dynamic_cast<MacrocosmScene*>(&prev)) {
        m->drop_ui();
    }

    Text::platform_retain_alphabet(pfrm);

    rotate_text_.emplace(
        pfrm, SYSTR(macro_rotate)->c_str(), OverlayCoord{3, 1});

    layers_text_.emplace(
        pfrm, SYSTR(macro_layers)->c_str(), OverlayCoord{3, 2});

    pfrm.set_tile(Layer::overlay, 1, 1, 394);
    pfrm.set_tile(Layer::overlay, 2, 1, 395);
    pfrm.set_tile(Layer::overlay, 1, 2, 392);
    pfrm.set_tile(Layer::overlay, 2, 2, 393);

    visible_layers_text_.emplace(
        pfrm, OverlayCoord{0, (u8)(calc_screen_tiles(pfrm).y - 1)});

    visible_layers_text_->assign(SYSTR(macro_visible_layers)->c_str());
    visible_layers_text_->append(app.macrocosm()->sector().get_z_view());
}



void ModifiedSelectorScene::exit(Platform& pfrm, App& app, Scene& next)
{
    MacrocosmScene::exit(pfrm, app, next);
    rotate_text_.reset();
    layers_text_.reset();
    pfrm.set_tile(Layer::overlay, 1, 1, 0);
    pfrm.set_tile(Layer::overlay, 2, 1, 0);
    pfrm.set_tile(Layer::overlay, 1, 2, 0);
    pfrm.set_tile(Layer::overlay, 2, 2, 0);
}



ScenePtr<Scene> ModifiedSelectorScene::update(Platform& pfrm,
                                              Player& player,
                                              macro::State& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    auto& sector = state.sector();

    if (player.key_pressed(pfrm, Key::alt_1) or
        player.key_pressed(pfrm, Key::alt_2)) {

        if (player.key_down(pfrm, Key::left)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        } else if (player.key_down(pfrm, Key::right)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            sector.rotate();
            sector.rotate();
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 2);
        } else if (player.key_down(pfrm, Key::down) and
                   sector.get_z_view() > 0) {
            bool success = sector.set_z_view(sector.get_z_view() - 1);
            if (not success) {
                pfrm.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                pfrm.speaker().play_sound("cursor_tick", 2);
            }
        } else if (player.key_down(pfrm, Key::up)) {
            bool success = sector.set_z_view(sector.get_z_view() + 1);
            if (not success) {
                pfrm.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                pfrm.speaker().play_sound("cursor_tick", 2);
            }
        }

    } else {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
