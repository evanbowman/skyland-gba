////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "zoneImageScene.hpp"
#include "adventureModeSettingsScene.hpp"
#include "highscoresScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/storm.hpp"
#include "worldMapScene.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void ZoneImageScene::enter(Scene& prev)
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
            PLATFORM.set_tile(Layer::map_0_ext, x, y, 0);
        }
    }

    PLATFORM.screen().set_shader(passthrough_shader);

    PLATFORM.screen().set_view(View{});
    PLATFORM.set_scroll(Layer::map_1_ext, 0, 8);
    PLATFORM.set_scroll(Layer::map_0_ext, 0, 0);

    if (APP.current_world_location() not_eq 0) {
        return;
    }

    const auto screen_tiles = calc_screen_tiles();
    for (int i = 0; i < screen_tiles.x; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 0, 112);
        PLATFORM.set_tile(Layer::overlay, i, 1, 112);
        PLATFORM.set_tile(Layer::overlay, i, 2, 116);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
        PLATFORM.set_tile(Layer::overlay, i, screen_tiles.y - 4, 256);
    }

    if (APP.zone() == 1) {
        PLATFORM.load_tile1_texture("zone_image_1_flattened");
    } else if (APP.zone() == 2) {
        PLATFORM.load_tile1_texture("zone_image_2_flattened");
    } else if (APP.zone() == 3) {
        PLATFORM.load_tile1_texture("zone_image_3_flattened");
    } else {
        PLATFORM.load_tile1_texture("zone_image_4_flattened");
    }

    __draw_image(1, 0, 3, 30, 14, Layer::map_1);

    PLATFORM.set_overlay_origin(0, 4);

    auto buffer = format<200>(SYSTR(zone_text)->c_str(), APP.zone());
    if (APP.zone() == 4) {
        buffer += SYSTR(final_zone)->c_str();
    }
    auto margin = centered_text_margins(buffer.length());
    text_.emplace(
        buffer.c_str(),
        OverlayCoord{u8(screen_tiles.x - (buffer.length() + margin + 1)),
                     u8(screen_tiles.y - 2)});
}



void ZoneImageScene::exit(Scene& next)
{
    PLATFORM.set_overlay_origin(0, 0);

    PLATFORM.fill_overlay(0);

    if (APP.player_island().interior_visible()) {
        auto t = APP.environment().player_island_interior_texture();
        PLATFORM.load_tile0_texture(t);
    } else {
        PLATFORM.load_tile0_texture(APP.environment().player_island_texture());
    }

    show_island_exterior(APP.opponent_island());

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
}



ScenePtr ZoneImageScene::update(Time delta)
{
    if (APP.current_world_location() not_eq 0) {
        return make_scene<WorldMapScene>();
    } else if (APP.zone() == 5) {
        return make_scene<HighscoresScene>(true, 1);
    }

    if (reset_nav_path_) {
        // We're entering a new zone. Clear out the old custom navigation
        // path...
        WorldMapScene::reset_nav_path();
    }

    switch (state_) {
    case State::fade_in: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            PLATFORM.screen().fade(0.f);
            state_ = State::wait;
            timer_ = 0;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }

    case State::wait:
        timer_ += delta;
        if (timer_ > seconds(3)) {
            state_ = State::fade_out;
            timer_ = 0;
        }
        break;

    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(1300);
        if (timer_ > fade_duration) {
            text_.reset();
            PLATFORM.screen().fade(
                1.f, ColorConstant::rich_black, {}, true, true);

            return make_scene<WorldMapScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }
    }

    return null_scene();
}



} // namespace skyland
