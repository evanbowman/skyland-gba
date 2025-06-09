////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "introCreditsScene.hpp"
#include "number/random.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "startAdventureScene.hpp"
#include "titleScreenScene.hpp"
#include "version.hpp"



namespace skyland
{



void IntroCreditsScene::enter(Scene& prev)
{
    PLATFORM.speaker().stream_music(TitleScreenScene::music_track()->c_str(),
                                    true);
    PLATFORM.speaker().play_sound("creaking", 9);

    PLATFORM.load_overlay_texture("overlay_skyland_title");
    PLATFORM.load_sprite_texture("spritesheet_title_screen");

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, false, false);

    rng::critical_state = 2021;

    if (APP.is_developer_mode()) {
        APP.start_console();
    }
}



void IntroCreditsScene::exit(Scene& next)
{
    PLATFORM.load_overlay_texture("overlay");

    PLATFORM.set_overlay_origin(0, 0);

    text_.reset();

    if (PLATFORM.device_name() == "PC") {
        APP.start_console();
    }
}



ScenePtr IntroCreditsScene::update(Time delta)
{
    timer_ += delta;

    if (wait_) {
        if (timer_ > milliseconds(500)) {
            wait_ = false;
            timer_ = 0;

            const auto text = SYSTR(intro_credits_name);

            const auto st = calc_screen_tiles();
            u8 margin = centered_text_margins(utf8::len(text->c_str()));

            text_.emplace(text->c_str(),
                          OverlayCoord{margin, (u8)(st.y / 2 - 3)});
        }
    } else if (text_) {
        if (timer_ > milliseconds(500) and timer_ < milliseconds(2000)) {
            auto amount =
                smoothstep(milliseconds(500), milliseconds(2000), timer_);
            PLATFORM.set_overlay_origin(0, amount * 30);
        }

        if (timer_ > milliseconds(2100)) {
            bird_seq_timer2_ += delta;
        }

        if (timer_ > milliseconds(1900)) {
            bird_seq_timer3_ += delta;
        }

        if (timer_ > milliseconds(1300)) {
            bird_seq_timer_ += delta;

            bird_anim_timer_ += delta;
            if (bird_anim_timer_ > milliseconds(150)) {
                bird_anim_timer_ -= milliseconds(150);
                ++bird_anim_;
                if (bird_anim_ == 6) {
                    bird_anim_ = 0;
                }
            }
        }

        if (timer_ > milliseconds(2000)) {
            PLATFORM.set_overlay_origin(0, 30);
            draw_image(82, 4, 9, 22, 10, Layer::overlay);
        }

        if (timer_ > milliseconds(3000) and not copyright_text_) {
            const auto cpystr = SYSTR(intro_credits_cpy);
            auto fmtcpystr = format(cpystr->c_str(),
                                    // NOTE: because we use yyyy.mm.dd
                                    // for version numbers.
                                    PROGRAM_MAJOR_VERSION);
            copyright_text_.emplace(

                OverlayCoord{
                    (u8)centered_text_margins(utf8::len(fmtcpystr.c_str())),
                    20});

            copyright_text_->assign(fmtcpystr.c_str(),
                                    FontColors{ColorConstant::med_blue_gray,
                                               ColorConstant::rich_black});
        }

        if (timer_ > milliseconds(5500) or key_down<Key::action_2>()) {
            text_.reset();
            copyright_text_.reset();
            PLATFORM.fill_overlay(0);
            timer_ = 0;
            bird_seq_timer_ = 0;
            bird_seq_timer2_ = 0;
            bird_seq_timer3_ = 0;
            if (key_down<Key::action_2>()) {
                skip_ = true;
            }
        }
    } else {
        if (skip_ or timer_ > milliseconds(400)) {
            PLATFORM.set_overlay_origin(0, 0);
            return make_scene<TitleScreenScene>();
        }
    }

    return null_scene();
}



void IntroCreditsScene::show_sunflowers(int scroll, Float darken)
{
}


void IntroCreditsScene::display()
{
    if (bird_seq_timer_) {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);

        Vec2<Float> pos;
        if (bird_seq_timer_ < milliseconds(1000)) {
            pos = interpolate(Vec2<Float>{191, 47},
                              Vec2<Float>{240, 16},
                              bird_seq_timer_ / Float(milliseconds(1000)));
            spr.set_texture_index(45 + bird_anim_);
        } else {
            spr.set_texture_index(44);
            pos.x = 191;
            pos.y = 47;
        }

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x), Fixnum(pos.y)});

        spr.set_priority(0);
        PLATFORM.screen().draw(spr);
    }

    if (bird_seq_timer2_) {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);

        Vec2<Float> pos;
        if (bird_seq_timer2_ < milliseconds(1000)) {
            pos = interpolate(Vec2<Float>{160, 64},
                              Vec2<Float>{240, 16},
                              bird_seq_timer2_ / Float(milliseconds(1000)));
            spr.set_texture_index(45 + bird_anim_);
        } else {
            spr.set_texture_index(44);
            pos.x = 160;
            pos.y = 64;
        }

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x), Fixnum(pos.y)});

        spr.set_priority(0);
        PLATFORM.screen().draw(spr);
    }

    if (bird_seq_timer3_) {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);

        Vec2<Float> pos;
        if (bird_seq_timer3_ < milliseconds(1000)) {
            pos = interpolate(Vec2<Float>{196, 72},
                              Vec2<Float>{240, 16},
                              bird_seq_timer3_ / Float(milliseconds(1000)));
            spr.set_texture_index(38 + bird_anim_);
        } else {
            spr.set_flip({true, false});
            spr.set_texture_index(37);
            pos.x = 196;
            pos.y = 72;
        }

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x), Fixnum(pos.y)});

        spr.set_priority(0);
        PLATFORM.screen().draw(spr);
    }
}



} // namespace skyland
