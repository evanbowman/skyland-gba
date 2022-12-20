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


#include "introCreditsScene.hpp"
#include "newgameScene.hpp"
#include "number/random.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



void IntroCreditsScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.speaker().play_music("shadows", true);
    pfrm.speaker().play_sound("creaking", 9);

    pfrm.load_overlay_texture("overlay_skyland_title");
    pfrm.load_sprite_texture("spritesheet_title_screen");

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, false, false);

    rng::critical_state = 2021;

    info(pfrm, "enter intro credits scene");

    if (app.is_developer_mode()) {
        app.start_console(pfrm);
    }
}



void IntroCreditsScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.load_overlay_texture("overlay");

    pfrm.set_overlay_origin(0, 0);

    text_.reset();
}



ScenePtr<Scene>
IntroCreditsScene::update(Platform& pfrm, App&, Microseconds delta)
{
    timer_ += delta;

    if (wait_) {
        flower_effect_timer_ += delta;
        if (timer_ > milliseconds(500)) {
            wait_ = false;
            timer_ = 0;

            const auto text = SYSTR(intro_credits_name);

            const auto st = calc_screen_tiles(pfrm);
            u8 margin = centered_text_margins(pfrm, utf8::len(text->c_str()));

            text_.emplace(
                pfrm, text->c_str(), OverlayCoord{margin, (u8)(st.y / 2 - 3)});
        }
    } else if (text_) {
        flower_effect_timer_ += delta;
        if (timer_ > milliseconds(500) and timer_ < milliseconds(2000)) {
            auto amount =
                smoothstep(milliseconds(500), milliseconds(2000), timer_);
            pfrm.set_overlay_origin(0, amount * 30);
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
            pfrm.set_overlay_origin(0, 30);
            draw_image(pfrm, 82, 4, 9, 22, 10, Layer::overlay);
        }

        if (timer_ > milliseconds(3000) and not copyright_text_) {
            const auto cpystr = SYSTR(intro_credits_cpy);
            copyright_text_.emplace(
                pfrm,
                OverlayCoord{
                    (u8)centered_text_margins(pfrm, utf8::len(cpystr->c_str())),
                    20});

            copyright_text_->assign(cpystr->c_str(),
                                    FontColors{ColorConstant::med_blue_gray,
                                               ColorConstant::rich_black});
        }

        auto t = pfrm.screen().touch();
        if (timer_ > milliseconds(5500) or key_down<Key::action_2>(pfrm) or
            (t and t->up_transition())) {
            text_.reset();
            copyright_text_.reset();
            pfrm.fill_overlay(0);
            timer_ = 0;
            bird_seq_timer_ = 0;
            bird_seq_timer2_ = 0;
            bird_seq_timer3_ = 0;
        }
    } else {
        if (timer_ > milliseconds(400)) {
            pfrm.set_overlay_origin(0, 0);
            return scene_pool::alloc<TitleScreenScene>();
        }
    }

    return null_scene();
}



void IntroCreditsScene::show_sunflowers(Platform& pfrm,
                                        int scroll,
                                        Float darken)
{
}


void IntroCreditsScene::display(Platform& pfrm, App& app)
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

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x),
                                      Fixnum(pos.y)});

        spr.set_priority(0);
        pfrm.screen().draw(spr);
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

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x),
                                      Fixnum(pos.y)});

        spr.set_priority(0);
        pfrm.screen().draw(spr);
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

        spr.set_position(Vec2<Fixnum>{Fixnum(pos.x),
                                      Fixnum(pos.y)});

        spr.set_priority(0);
        pfrm.screen().draw(spr);
    }
}



} // namespace skyland
