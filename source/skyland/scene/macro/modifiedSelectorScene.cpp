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



void ModifiedSelectorScene::enter(Platform& pfrm,
                                  macro::EngineImpl& state,
                                  Scene& prev)
{
    if (auto m = prev.cast_macrocosm_scene()) {
        m->drop_ui();
    }

    pfrm.screen().schedule_fade(0.f);

    Text::platform_retain_alphabet(pfrm);

    day_night_text_.emplace(pfrm, OverlayCoord{1, 1});
    day_night_text_->assign(
        "b", FontColors{custom_color(0xa3c447), ColorConstant::rich_black});
    day_night_text_->append(SYSTR(macro_day_or_night)->c_str());

    cursor_text_.emplace(pfrm, OverlayCoord{1, 4});
    cursor_text_->assign(
        "a", FontColors{custom_color(0xa3c447), ColorConstant::rich_black});
    cursor_text_->append(SYSTR(macro_raise)->c_str());

    rotate_text_.emplace(
        pfrm, SYSTR(macro_rotate)->c_str(), OverlayCoord{3, 2});

    layers_text_.emplace(
        pfrm, SYSTR(macro_layers)->c_str(), OverlayCoord{3, 3});

    pfrm.set_tile(Layer::overlay, 1, 2, 394);
    pfrm.set_tile(Layer::overlay, 2, 2, 395);
    pfrm.set_tile(Layer::overlay, 1, 3, 392);
    pfrm.set_tile(Layer::overlay, 2, 3, 393);

    visible_layers_text_.emplace(
        pfrm, OverlayCoord{0, (u8)(calc_screen_tiles(pfrm).y - 1)});

    visible_layers_text_->assign(SYSTR(macro_visible_layers)->c_str());
    visible_layers_text_->append(state.sector().get_z_view());
}



void ModifiedSelectorScene::exit(Platform& pfrm,
                                 macro::EngineImpl& state,
                                 Scene& next)
{
    MacrocosmScene::exit(pfrm, state, next);
    rotate_text_.reset();
    layers_text_.reset();
    day_night_text_.reset();
    visible_layers_text_.reset();
    cursor_text_.reset();
    pfrm.set_tile(Layer::overlay, 1, 2, 0);
    pfrm.set_tile(Layer::overlay, 2, 2, 0);
    pfrm.set_tile(Layer::overlay, 1, 3, 0);
    pfrm.set_tile(Layer::overlay, 2, 3, 0);
}



ScenePtr<Scene> ModifiedSelectorScene::update(Platform& pfrm,
                                              Player& player,
                                              macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(pfrm, player, state)) {
        return scene;
    }

    auto test_key = [&](Key k) {
        return player.test_key(pfrm, k, milliseconds(500), milliseconds(100));
    };

    auto& sector = state.sector();

    if (player.key_pressed(pfrm, Key::alt_1) or
        player.key_pressed(pfrm, Key::alt_2)) {

        if (player.key_down(pfrm, Key::left)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            sector.render(pfrm);
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(pfrm, Key::right)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            sector.rotate();
            sector.rotate();
            sector.render(pfrm);
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
        } else if (test_key(Key::down) and sector.get_z_view() > 0) {
            bool success = sector.set_z_view(sector.get_z_view() - 1);
            if (not success) {
                pfrm.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::up)) {
            bool success = sector.set_z_view(sector.get_z_view() + 1);
            if (not success) {
                pfrm.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                pfrm.speaker().play_sound("cursor_tick", 0);
            }
        } else if (player.key_down(pfrm, Key::action_2)) {
            // pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            // pfrm.screen().clear();
            // pfrm.screen().display();
            // raster::globalstate::is_night = not raster::globalstate::is_night;
            // if (raster::globalstate::is_night) {
            //     pfrm.load_background_texture("background_macro_night");
            // } else {
            //     pfrm.load_background_texture("background_macro");
            // }
            // raster::globalstate::_recalc_depth_test.fill();
            // sector.shadowcast();
            // raster::globalstate::_changed = true;
            // sector.render(pfrm);
            // pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            // pfrm.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(pfrm, Key::action_1)) {
            auto c = sector.cursor();
            if (c.z < sector.size().z - 1) {
                ++c.z;
                auto block = state.sector().get_block(c);
                while (block.type_ not_eq (u8) terrain::Type::air) {
                    ++c.z;
                    block = state.sector().get_block(c);
                }
                if (c.z <= sector.size().z - 1) {
                    pfrm.speaker().play_sound("cursor_tick", 0);
                    state.sector().set_cursor(c, false);
                } else {
                    pfrm.speaker().play_sound("beep_error", 2);
                }

            } else {
                pfrm.speaker().play_sound("beep_error", 2);
            }
        }

    } else {
        return scene_pool::alloc<SelectorScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
