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
                                            "",
                                            "QR Code Generator by",
                                            "Project Nayuki",
                                            "",
                                            "Heatshrink Algorithm by",
                                            "Atomic Object",
                                            "",
                                            "",
                                            "",
                                            nullptr};

static const char* const page_02_lines[] = {"-- Programming (contd.) --",
                                            "",
                                            "Thanks to Martin Korth",
                                            "for the GBATEK hardware docs",
                                            "",
                                            "Thanks to the GBA",
                                            "homebrew dev community",
                                            "",
                                            "Thanks to Endrift",
                                            "for the mGBA Emulator",
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
                                           "\x12 Struttin With Some BBQ \x12",
                                           "Louis Armstrong",
                                           "",
                                           "",
                                           "-- Sounds --",
                                           "",
                                           "\x12 Interface Sounds \x12",
                                           "The Boom Library",
                                           nullptr};

static const char* const page_4_lines[] = {"-- Sounds (Contd.) --",
                                           "",
                                           "\x12 misc. effects \x12",
                                           "Evan + The Public Domain",
                                           "",
                                           "(Gathered public-domain",
                                           "recordings of nature, bells,",
                                           "and machinery, and remixed",
                                           "them to make the game's sfx)",
                                           nullptr};


static const char* const page_5_lines[] = {"-- Beta Testing --",
                                           "",
                                           "junnunkarim",
                                           "tolik518",
                                           "Ben Casler",
                                           nullptr};


static const char* const page_end_lines0[] = {"-- Postscript --",
                                              "",
                                              "Making games is so hard...",
                                              "",
                                              "",
                                              "",
                                              "",
                                              "",
                                              "",
                                              nullptr};

static const char* const page_end_lines1[] = {"-- Postscript --",
                                              "",
                                              "Although it's only a simple",
                                              "game, Skyland took thousands",
                                              "of hours of work.",
                                              "",
                                              "It's really difficult to",
                                              "call a project finished,",
                                              "especially after working on",
                                              "it for months.",
                                              nullptr};

static const char* const page_end_lines2[] = {"-- Postscript --",
                                              "",
                                              "But I'm running low on",
                                              "cartridge space, so I guess",
                                              "now is as good a time as any",
                                              "to publish.",
                                              "",
                                              "As only my second game, I'm",
                                              "excited to share it with",
                                              "everyone!",
                                              "",
                                              nullptr};

static const char* const page_end_lines3[] = {"-- Postscript --",
                                              "",
                                              "",
                                              "Thanks for playing!",
                                              "",
                                              "-- Evan",
                                              "",
                                              "",
                                              "",
                                              "",
                                              "",
                                              nullptr};



static const std::array<Microseconds, 11> page_times_ = {
    milliseconds(3000),
    milliseconds(3000),
    milliseconds(3000),
    milliseconds(1500),
    milliseconds(3000),
    milliseconds(3000),
    milliseconds(4500),
    milliseconds(1000),
    milliseconds(5000),
    milliseconds(5000),
    milliseconds(2000),
};


static const std::array<Page, 12> pages_ = {{{page_0_lines},
                                             {page_01_lines},
                                             {page_02_lines},
                                             {page_1_lines},
                                             {page_2_lines},
                                             {page_3_lines},
                                             {page_4_lines},
                                             {page_5_lines},
                                             {page_end_lines0},
                                             {page_end_lines1},
                                             {page_end_lines2},
                                             {page_end_lines3}}};



void CreditsModule::load_page(u32 page)
{
    if (page >= pages_.size()) {
        return;
    }

    lines_.clear();

    PLATFORM.fill_overlay(112);

    u8 y = 1;
    auto data = pages_[page].text_lines_;

    while (*data not_eq nullptr) {
        if (str_len(*data) == 0) {
            y += 1;
            ++data;
            if (*data == nullptr) {
                break;
            }
        }
        lines_.emplace_back(

            *data,
            OverlayCoord{(u8)centered_text_margins(utf8::len(*data)), y});
        y += 2;
        ++data;
    }
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



ScenePtr<Scene> CreditsModule::update(Microseconds delta)
{
    constexpr auto fade_duration = milliseconds(650);


    switch (state_) {
    case State::idle:
        if (autoadvance_) {
            timer_ += delta;
        }
        if ((autoadvance_ and timer_ > page_times_[page_]) or
            player().key_down(Key::action_1)) {
            if (player().key_down(Key::action_2) or
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
            return scene_pool::alloc<TitleScreenScene>(3);
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(
                amount, ColorConstant::rich_black, true, true);
        }
        break;

    case State::page_swap:
        load_page(++page_);
        state_ = State::fade_in;
        break;
    }

    return null_scene();
}



CreditsModule::Factory CreditsModule::factory_;



} // namespace skyland
