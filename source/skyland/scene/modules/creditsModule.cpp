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


#include "creditsModule.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



using Line = const char* const;



struct Page
{
    Line* text_lines_;
};


static const char* const page_0_lines[] = {"-- Programming --",
                                           "",
                                           "< Lead programmer >",
                                           "Evan Bowman",
                                           "",
                                           "interrupt dispatcher by",
                                           "Dave Murphy (devkitpro)",
                                           "",
                                           "optimized memcpy by",
                                           "Jasper Vijn (tonc)",
                                           nullptr};



static const char* const page_01_lines[] = {"-- Programming (contd.) --",
                                            "",
                                            "Thanks to the GBA",
                                            "homebrew dev community",
                                            "",
                                            "",
                                            "",
                                            "",
                                            nullptr};



static const char* const page_1_lines[] = {"-- Design --",
                                           "",
                                           "Design and Artwork",
                                           "Evan Bowman",
                                           nullptr};



static const char* const page_2_lines[] = {"-- Music --",
                                           "",
                                           "\x12 Shadows \x12",
                                           "Unattributed",
                                           "",
                                           "\x12 Life in Silico \x12",
                                           "Scott Buckley",
                                           "",
                                           "\x12 Solecism \x12",
                                           "Scott Buckley",
                                           nullptr};



static const char* const page_3_lines[] = {"-- Music (Contd.) --",
                                           "",
                                           "\x12 Struttin (sample) \x12",
                                           "Louis Armstrong",
                                           "",
                                           "\x12 Interface Sounds \x12",
                                           "The Boom Library",
                                           nullptr};


static const char* const page_end_lines[] = {"-- P.S. --",
                                             "",
                                             "I worked on this project",
                                             "Obsessively for months.",
                                             "",
                                             "Making games is so hard!",
                                             "",
                                             "Please enjoy!",
                                             "",
                                             "-- Evan",
                                             nullptr};




std::array<Page, 6> pages_ = {{{page_0_lines}, {page_01_lines}, {page_1_lines}, {page_2_lines}, {page_3_lines}, {page_end_lines}}};



void CreditsModule::load_page(Platform& pfrm, u32 page)
{
    if (page >= pages_.size()) {
        return;
    }

    lines_.clear();

    pfrm.fill_overlay(112);

    u8 y = 1;
    auto data = pages_[page].text_lines_;

    while (*data not_eq nullptr) {
        if (str_len(*data) == 0) {
            y += 1;
            ++data;
        }
        lines_.emplace_back(
            pfrm,
            *data,
            OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(*data)), y});
        y += 2;
        ++data;
    }
}



void CreditsModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    load_page(pfrm, 0);
    pfrm.set_overlay_origin(0, -4);

    pfrm.speaker().set_music_volume(10);
}



void CreditsModule::exit(Platform& pfrm, App& app, Scene& next)
{
    lines_.clear();
    pfrm.fill_overlay(0);
    pfrm.set_overlay_origin(0, 0);

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);
}



ScenePtr<Scene>
CreditsModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    constexpr auto fade_duration = milliseconds(650);


    switch (state_) {
    case State::idle:
        if (player(app).key_down(pfrm, Key::action_1)) {
            if (player(app).key_down(pfrm, Key::action_2) or
                (u32) page_ + 1 == ::skyland::pages_.size()) {
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
            pfrm.screen().schedule_fade(0.f);
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
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
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;

    case State::fade_out_exit:
        timer_ += delta;
        if (timer_ > fade_duration) {
            return scene_pool::alloc<TitleScreenScene>(3);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;

    case State::page_swap:
        load_page(pfrm, ++page_);
        state_ = State::fade_in;
        break;
    }

    return null_scene();
}



CreditsModule::Factory CreditsModule::factory_;



} // namespace skyland
