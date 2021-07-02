#include "zoneImageScene.hpp"
#include "localization.hpp"
#include "newgameScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland {



void __draw_image(Platform& pfrm,
                  TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



void ZoneImageScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().set_view(View{});
    pfrm.set_scroll(Layer::map_1_ext, 0, 8);
    pfrm.set_scroll(Layer::map_0_ext, 0, 0);

    if (not(app.current_map_location().x == 0 and
            app.current_map_location().y == 1)) {
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
        pfrm.load_tile1_texture("zone_image_0_flattened");
    } else {
        pfrm.load_tile1_texture("zone_image_1_flattened");
    }

    __draw_image(pfrm, 1, 0, 3, 30, 14, Layer::map_1);

    pfrm.set_overlay_origin(0, 4);

    StringBuffer<32> buffer("-- zone ");
    buffer += to_string<10>(app.zone());
    buffer += " --";
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
        pfrm.load_tile0_texture("tilesheet_interior");
    } else {
        pfrm.load_tile0_texture("tilesheet");

        u8 test[16][16];
        for (int i = 0; i < 16; ++i) {
            for (int j = 0; j < 16; ++j) {
                if (j % 2) {
                    test[i][j] = 1;
                } else {
                    test[i][j] = 4;
                }
            }
        }

        pfrm.encode_tile(test);
        // pfrm.overwrite_t0_tile(105, t);
    }

    pfrm.load_tile1_texture("tilesheet_enemy_0");

    for (int x = 0; x < 16; ++x) {
        for (int y = 0; y < 16; ++y) {
            pfrm.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
}



ScenePtr<Scene>
ZoneImageScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not(app.current_map_location().x == 0 and
            app.current_map_location().y == 1)) {
        return scene_pool::alloc<WorldMapScene>();
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
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
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
            return scene_pool::alloc<WorldMapScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(
                amount, ColorConstant::rich_black, {}, true, true);
        }
        break;
    }
    }

    return null_scene();
}



} // namespace skyland
