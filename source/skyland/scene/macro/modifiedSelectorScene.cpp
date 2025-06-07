////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////



#include "modifiedSelectorScene.hpp"
#include "selectorScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland::macro
{



void ModifiedSelectorScene::enter(macro::EngineImpl& state, Scene& prev)
{
    if (auto m = prev.cast_macrocosm_scene()) {
        m->drop_ui();
    }

    PLATFORM.screen().schedule_fade(0.f);

    cursor_text_.emplace(OverlayCoord{1, 3});
    cursor_text_->assign(
        "a", FontColors{custom_color(0xa3c447), ColorConstant::rich_black});
    cursor_text_->append(SYSTR(macro_raise)->c_str());

    rotate_text_.emplace(SYSTR(macro_rotate)->c_str(), OverlayCoord{3, 1});

    layers_text_.emplace(SYSTR(macro_layers)->c_str(), OverlayCoord{3, 2});

    PLATFORM.set_tile(Layer::overlay, 1, 1, 394);
    PLATFORM.set_tile(Layer::overlay, 2, 1, 395);
    PLATFORM.set_tile(Layer::overlay, 1, 2, 392);
    PLATFORM.set_tile(Layer::overlay, 2, 2, 393);

    visible_layers_text_.emplace(
        OverlayCoord{0, (u8)(calc_screen_tiles().y - 1)});

    visible_layers_text_->assign(SYSTR(macro_visible_layers)->c_str());
    visible_layers_text_->append(state.sector().get_z_view());
}



void ModifiedSelectorScene::exit(macro::EngineImpl& state, Scene& next)
{
    MacrocosmScene::exit(state, next);
    rotate_text_.reset();
    layers_text_.reset();
    visible_layers_text_.reset();
    cursor_text_.reset();
    PLATFORM.set_tile(Layer::overlay, 1, 1, 0);
    PLATFORM.set_tile(Layer::overlay, 2, 1, 0);
    PLATFORM.set_tile(Layer::overlay, 1, 2, 0);
    PLATFORM.set_tile(Layer::overlay, 2, 2, 0);
}



ScenePtr ModifiedSelectorScene::update(Player& player, macro::EngineImpl& state)
{
    if (auto scene = MacrocosmScene::update(player, state)) {
        return scene;
    }

    auto test_key = [&](Key k) {
        return player.test_key(k, milliseconds(500), milliseconds(100));
    };

    auto& sector = state.sector();

    if (player.key_pressed(Key::alt_1) or player.key_pressed(Key::alt_2)) {

        if (player.key_down(Key::left)) {
            PLATFORM.screen().schedule_fade(0.7f, custom_color(0x102447));
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            sector.rotate();
            sector.render();
            PLATFORM.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(Key::right)) {
            PLATFORM.screen().schedule_fade(0.7f, custom_color(0x102447));
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            sector.rotate();
            sector.rotate();
            sector.rotate();
            sector.render();
            PLATFORM.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (test_key(Key::down) and sector.get_z_view() > 0) {
            bool success = sector.set_z_view(sector.get_z_view() - 1);
            if (not success) {
                PLATFORM.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (test_key(Key::up)) {
            bool success = sector.set_z_view(sector.get_z_view() + 1);
            if (not success) {
                PLATFORM.speaker().play_sound("beep_error", 2);
            } else {
                visible_layers_text_->assign(
                    SYSTR(macro_visible_layers)->c_str());
                visible_layers_text_->append(state.sector().get_z_view());
                PLATFORM.speaker().play_sound("cursor_tick", 0);
            }
        } else if (player.key_down(Key::action_2)) {
            // PLATFORM.screen().schedule_fade(0.7f, custom_color(0x102447));
            // PLATFORM.screen().clear();
            // PLATFORM.screen().display();
            // raster::globalstate::is_night = not raster::globalstate::is_night;
            // if (raster::globalstate::is_night) {
            //     PLATFORM.load_background_texture("background_macro_night");
            // } else {
            //     PLATFORM.load_background_texture("background_macro");
            // }
            // raster::globalstate::_recalc_depth_test.fill();
            // sector.shadowcast();
            // raster::globalstate::_changed = true;
            // sector.render();
            // PLATFORM.screen().schedule_fade(0.f, ColorConstant::rich_black);
            // PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(Key::action_1)) {
            auto c = sector.cursor();
            if (c.z < sector.size().z - 1) {
                ++c.z;
                auto block = state.sector().get_block(c);
                while (block.type_ not_eq (u8) terrain::Type::air) {
                    ++c.z;
                    block = state.sector().get_block(c);
                }
                if (c.z <= sector.size().z - 1) {
                    PLATFORM.speaker().play_sound("cursor_tick", 0);
                    state.sector().set_cursor(c, false);
                } else {
                    PLATFORM.speaker().play_sound("beep_error", 2);
                }

            } else {
                PLATFORM.speaker().play_sound("beep_error", 2);
            }
        }

    } else {
        return make_scene<SelectorScene>();
    }

    return null_scene();
}



} // namespace skyland::macro
