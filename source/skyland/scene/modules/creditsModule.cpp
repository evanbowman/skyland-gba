////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "creditsModule.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



bool CreditsModule::load_page(u32 page)
{
    Vector<char> data;

    const char* fmt_path = "/scripts/data/credits/%.txt";
    if (not APP.load_file(format(fmt_path, page).c_str(), data)) {
        return false;
    }

    lines_.clear();

    PLATFORM.fill_overlay(112);

    u8 y = 1;

    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 112);
        }
    }

    StringBuffer<128> line;
    for (char c : data) {
        if (c == '\n') {
            if (line.length()) {
                if (line[0] == '/') {
                    PLATFORM.load_tile1_texture(line.c_str());
                    PLATFORM.screen().set_view(View{});
                    __draw_image(0, 3, y, 24, 10, Layer::map_1);
                    PLATFORM.set_scroll(Layer::map_1_ext, 0, -(y - 2) * 8 + 4);
                    for (int x = 0; x < 30; ++x) {
                        for (int yy = 0; yy < 20; ++yy) {
                            if (not(yy < y or yy > y + 9 or x < 3 or x > 26)) {
                                PLATFORM.set_tile(Layer::overlay, x, yy, 0);
                            }
                        }
                    }
                    y += 10;
                    line.clear();
                    continue;
                }
                lines_.emplace_back(
                    line.c_str(),
                    OverlayCoord{
                        (u8)centered_text_margins(utf8::len(line.c_str())), y});
                line.clear();
            }
            y += 1;
        } else if (c == '^') {
            line.push_back('\x12');
        } else {
            line.push_back(c);
        }
    }

    return true;
}



void CreditsModule::enter(Scene& prev)
{
    load_page(0);
    PLATFORM.set_overlay_origin(0, -4);

    PLATFORM.speaker().set_music_volume(10);
}



void CreditsModule::exit(Scene& next)
{
    lines_.clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.set_overlay_origin(0, 0);

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



ScenePtr CreditsModule::update(Time delta)
{
    constexpr auto fade_duration = milliseconds(650);


    switch (state_) {
    case State::idle:
        if (player().key_down(Key::action_2)) {
            state_ = State::fade_out_exit;
        }
        if (player().key_down(Key::action_1)) {
            if (player().key_down(Key::action_2)) {
                state_ = State::fade_out_exit;
            } else {
                state_ = State::fade_out_next;
            }
            timer_ = 0;
        }
        break;

    case State::fade_in:
        timer_ += delta;
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::idle;
            PLATFORM.screen().schedule_fade(
                0.f, ColorConstant::rich_black, true, true, true, true);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;

    case State::fade_out_next:
        timer_ += delta;
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::page_swap;
            PLATFORM.screen().schedule_fade(
                1, ColorConstant::rich_black, true, true);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true, true, true);
        }
        break;

    case State::fade_out_exit:
        timer_ += delta;
        if (timer_ > fade_duration) {
            if (next_scene_) {
                return (*next_scene_)();
            }
            return make_scene<TitleScreenScene>(3);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true, true, false);
        }
        break;

    case State::page_swap:
        if (not load_page(++page_)) {
            return make_scene<TitleScreenScene>(3);
        }
        state_ = State::fade_in;
        break;
    }

    return null_scene();
}



CreditsModule::Factory CreditsModule::factory_;



} // namespace skyland
