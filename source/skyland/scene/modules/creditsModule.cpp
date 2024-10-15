////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////


#include "creditsModule.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



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

    StringBuffer<128> line;
    for (char c : data) {
        if (c == '\n') {
            if (line.length()) {
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
        if (autoadvance_) {
            timer_ += delta;
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
            PLATFORM.screen().schedule_fade(0.f);
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
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
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
                amount, ColorConstant::rich_black, true, true);
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
