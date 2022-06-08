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


#include "zoneImageScene.hpp"
#include "highscoresScene.hpp"
#include "newgameScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/storm.hpp"
#include "worldMapScene.hpp"



namespace skyland
{



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void ZoneImageScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }

    pfrm.screen().set_view(View{});
    pfrm.set_scroll(Layer::map_1_ext, 0, 8);
    pfrm.set_scroll(Layer::map_0_ext, 0, 0);

    if (app.zone() > 2) {
        app.swap_environment<weather::Storm>();
    } else {
        app.swap_environment<weather::ClearSkies>();
    }

    if (not app.current_world_location() == 0) {
        return;
    }

    const auto screen_tiles = calc_screen_tiles(pfrm);
    for (int i = 0; i < screen_tiles.x; ++i) {
        pfrm.set_tile(Layer::overlay, i, 0, 112);
        pfrm.set_tile(Layer::overlay, i, 1, 112);
        pfrm.set_tile(Layer::overlay, i, 2, 116);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 1, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 2, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 3, 112);
        pfrm.set_tile(Layer::overlay, i, screen_tiles.y - 4, 256);
    }

    if (app.zone() == 1) {
        pfrm.load_tile1_texture("zone_image_1_flattened");
    } else if (app.zone() == 2) {
        pfrm.load_tile1_texture("zone_image_2_flattened");
    } else {
        pfrm.load_tile1_texture("zone_image_3_flattened");
    }

    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_1);

    pfrm.set_overlay_origin(0, 4);

    auto buffer = format<32>(SYSTR(zone_text)->c_str(), app.zone());
    auto margin = centered_text_margins(pfrm, buffer.length());
    text_.emplace(
        pfrm,
        buffer.c_str(),
        OverlayCoord{u8(screen_tiles.x - (buffer.length() + margin + 1)),
                     u8(screen_tiles.y - 2)});
}



void ZoneImageScene::exit(Platform& pfrm, App& app, Scene& next)
{
    pfrm.set_overlay_origin(0, 0);

    pfrm.fill_overlay(0);

    if (app.player_island().interior_visible()) {
        auto t = app.environment().player_island_interior_texture();
        pfrm.load_tile0_texture(t);
    } else {
        pfrm.load_tile0_texture(app.environment().player_island_texture());
    }

    show_island_exterior(pfrm, app, app.opponent_island());

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
}



ScenePtr<Scene>
ZoneImageScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.current_world_location() == 0) {
        return scene_pool::alloc<WorldMapScene>();
    } else if (app.zone() == 5) {
        return scene_pool::alloc<HighscoresScene>(true, 1);
    }

    switch (state_) {
    case State::fade_in: {
        timer_ += delta;

        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            state_ = State::wait;
            timer_ = 0;
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
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
            pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
            return scene_pool::alloc<WorldMapScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;
    }
    }

    return null_scene();
}



} // namespace skyland
