////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "setGamespeedScene.hpp"
#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "swapOverlayTextureScene.hpp"



namespace skyland
{



static const SystemStringBuffer gamespeed_text(GameSpeed speed)
{
    switch (speed) {
    case GameSpeed::stopped:
        return SYSTR(gs_paused);
    case GameSpeed::slow:
        return SYSTR(gs_slow);
    case GameSpeed::normal:
        return SYSTR(gs_regular);
    case GameSpeed::fast:
        return SYSTR(gs_fast);
    case GameSpeed::rewind:
        return SYSTR(gs_rewind);
    case GameSpeed::count:
        break;
    }
    return SYSTR(gs_error);
}



ScenePtr SetGamespeedScene::update(Time delta)
{
    APP.player().update(delta);

    if (APP.player().key_down(Key::right)) {
        selection_++;
        selection_ %= (int)GameSpeed::count;
        repaint_selector();
    } else if (APP.player().key_down(Key::left)) {
        selection_--;
        if (selection_ < 0) {
            selection_ = (int)GameSpeed::count - 1;
        }
        repaint_selector();
    }

    if ((button_mode_ == 0 and APP.player().key_up(Key::alt_1)) or
        (button_mode_ == 1 and APP.player().key_down(Key::action_1))) {
        if ((GameSpeed)selection_ == GameSpeed::rewind) {
            if (APP.time_stream().pushes_enabled()) {
                set_gamespeed(GameSpeed::stopped);

                return make_scene<SwapOverlayTextureScene>(
                    "overlay",
                    make_deferred_scene<RewindScene>(is_far_camera()));
            } else {
                set_gamespeed(GameSpeed::stopped);
                selection_ = (int)GameSpeed::stopped;
                repaint_selector();

                return make_scene<SwapOverlayTextureScene>("overlay", [] {
                    auto future_scene = []() {
                        return make_scene<ReadyScene>();
                    };
                    const char* msg = "rewind disabled!";
                    return make_scene<NotificationScene>(msg, future_scene);
                });
            }
        } else {
            set_gamespeed((GameSpeed)selection_);

            return make_scene<SwapOverlayTextureScene>(
                "overlay", make_deferred_scene<ReadyScene>());
        }
    }

    if (button_mode_ == 1 and APP.player().key_down(Key::action_2)) {
        return make_scene<SwapOverlayTextureScene>(
            "overlay", make_deferred_scene<ReadyScene>());
    }

    return null_scene();
}



u16 gamespeed_icon(GameSpeed speed);



void SetGamespeedScene::enter(Scene& scene)
{
    selection_ = (int)APP.game_speed();

    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay_gamespeed");

    repaint_selector();

    PLATFORM.speaker().set_music_speed(Platform::Speaker::MusicSpeed::regular);

    if (auto ws = scene.cast_world_scene()) {
        if (ws->is_far_camera()) {
            far_camera();
        }
    }
}



void SetGamespeedScene::repaint_selector()
{
    auto st = calc_screen_tiles();
    for (int i = 0; i < (int)GameSpeed::count; ++i) {
        auto gs = (GameSpeed)((i + selection_) % (int)GameSpeed::count);
        auto t = gamespeed_icon(gs);

        auto start = st.x - 3;

        if (i > 0) {
            start -= 1;
        }

        PLATFORM.set_tile(Layer::overlay, start - i * 2, 1, t++);
        PLATFORM.set_tile(Layer::overlay, start - i * 2 + 1, 1, t++);
        PLATFORM.set_tile(Layer::overlay, start - i * 2, 2, t++);
        PLATFORM.set_tile(Layer::overlay, start - i * 2 + 1, 2, t);

        PLATFORM.set_tile(Layer::overlay, start - i * 2, 3, 119);
        PLATFORM.set_tile(Layer::overlay, start - i * 2 + 1, 3, 119);
    }


    PLATFORM.set_tile(
        Layer::overlay, (st.x - 5) - 2 * ((int)GameSpeed::count - 1), 1, 424);


    PLATFORM.set_tile(
        Layer::overlay, (st.x - 5) - 2 * ((int)GameSpeed::count - 1), 2, 128);


    PLATFORM.set_tile(Layer::overlay, (st.x - 1), 1, 423);


    PLATFORM.set_tile(Layer::overlay, (st.x - 1), 2, 433);

    // divider
    PLATFORM.set_tile(Layer::overlay, (st.x - 4), 1, 379);


    PLATFORM.set_tile(Layer::overlay, (st.x - 4), 2, 379);

    PLATFORM.set_tile(Layer::overlay, (st.x - 4), 3, 119);

    if (not speed_text_) {
        speed_text_.emplace(OverlayCoord{0, u8(calc_screen_tiles().y - 1)});
    }
    StringBuffer<30> temp(SYSTR(gs_prompt)->c_str());
    temp += gamespeed_text((GameSpeed)selection_)->c_str();
    speed_text_->assign(temp.c_str());

    const u8 y = calc_screen_tiles().y - 2;

    for (int i = 0; i < 30; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, y, 0);
    }

    for (int i = 0; i < speed_text_->len(); ++i) {
        PLATFORM.set_tile(Layer::overlay, i, y, 425);
    }
}



void SetGamespeedScene::exit(Scene&)
{
    speed_text_.reset();
    PLATFORM.fill_overlay(0);
}



} // namespace skyland
