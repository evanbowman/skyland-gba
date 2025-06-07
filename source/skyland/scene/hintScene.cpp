////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "hintScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "worldMapScene.hpp"



namespace skyland
{


struct HintInfo
{
    const char* img_name_;
    SystemString text_;
};



static const std::array<HintInfo, 13> hints = {
    {{"<none>", SystemString::hint_boss},
     {"<none>", SystemString::hint_nav_plot},
     {"<none>", SystemString::hint_gamespeed},
     {"hint_infirmary", SystemString::hint_infirmary},
     {"<none>", SystemString::hint_navigation},
     {"hint_goblin", SystemString::hint_doors},
     {"hint_goblin", SystemString::hint_repair},
     {"<none>", SystemString::hint_hotkeys},
     {"<none>", SystemString::hint_glossary},
     {"hint_plunder", SystemString::hint_plunder},
     {"hint_damaged_core", SystemString::hint_damaged_core},
     {"<none>", SystemString::hint_radar},
     {"<none>", SystemString::hint_tutorials}}};



static void show_hint(const HintInfo& info, TextView& text)
{
    if (str_cmp("<none>", info.img_name_) == 0) {
        PLATFORM.load_overlay_texture("hint_infirmary");
        text.assign(loadstr(info.text_)->c_str(), {2, 4}, {26, 18}, 0);
    } else {
        PLATFORM.load_overlay_texture(info.img_name_);
        draw_image(82, 18, 4, 12, 10, Layer::overlay);
        text.assign(loadstr(info.text_)->c_str(), {2, 6}, {16, 12}, 0);
    }
}



void HintScene::enter(Scene& prev)
{
    PLATFORM.fill_overlay(82);

    show_hint(hints[hint_index_], *body_);

    heading_.emplace(OverlayCoord{1, 1});
    heading_->assign(
        SYSTR(hint_title)->c_str(),
        Text::OptColors{{custom_color(0x163061), custom_color(0xffffff)}});
}


void HintScene::exit(Scene& next)
{
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
    heading_.reset();
    body_.reset();
    PLATFORM.fill_overlay(0);
}


ScenePtr HintScene::update(Time delta)
{
    switch (state_) {
    case State::scene_intro: {
        static const auto fade_duration = milliseconds(400);
        timer_ += delta;
        if (timer_ < fade_duration) {
            PLATFORM.screen().schedule_fade(
                1.f - smoothstep(0.f, fade_duration, timer_),
                ColorConstant::rich_black,
                true,
                true);
        } else {
            PLATFORM.screen().fade(0.f);
            timer_ = 0;
            state_ = State::idle;
        }
        break;
    }

    case State::idle:
        if (APP.player().key_down(Key::action_1)) {
            // if (hint_index_ >= (sizeof(hints) / sizeof(HintInfo))) {
            //     hint_index_ = 0;
            // } else {
            // }
            ++hint_index_;
            hint_index_ %= hints.size();
            heading_.reset();
            timer_ = 0;

            state_ = State::fade_out;
        } else if (APP.player().key_down(Key::action_2)) {
            return make_scene<WorldMapScene>();
        }
        break;

    case State::fade_in: {
        static const auto fade_duration = milliseconds(200);
        timer_ += delta;
        if (timer_ < fade_duration) {
            PLATFORM.screen().fade(1.f - smoothstep(0.f, fade_duration, timer_),
                                   custom_color(0x163061),
                                   {},
                                   true,
                                   true);
        } else {
            PLATFORM.screen().fade(0.f);
            timer_ = 0;
            state_ = State::idle;
        }
        break;
    }

    case State::swap_img:
        body_.reset();
        PLATFORM.fill_overlay(82);

        body_.emplace();
        draw_image(82, 18, 4, 12, 10, Layer::overlay);
        show_hint(hints[hint_index_], *body_);
        state_ = State::fade_in;

        heading_.emplace(OverlayCoord{1, 1});
        heading_->assign(
            SYSTR(hint_title)->c_str(),
            Text::OptColors{{custom_color(0x163061), custom_color(0xffffff)}});
        break;

    case State::fade_out:
        static const auto fade_duration = milliseconds(200);
        timer_ += delta;
        if (timer_ < fade_duration) {
            PLATFORM.screen().fade(smoothstep(0.f, fade_duration, timer_),
                                   custom_color(0x163061),
                                   {},
                                   true,
                                   true);
        } else {
            PLATFORM.screen().fade(1.f, custom_color(0x163061), {}, true, true);

            timer_ = 0;
            state_ = State::swap_img;
        }
        break;
    }

    return null_scene();
}



} // namespace skyland
