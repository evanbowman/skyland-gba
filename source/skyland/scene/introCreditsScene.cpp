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
#include "skyland/systemString.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



void IntroCreditsScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.speaker().play_music("shadows", true);
    pfrm.speaker().play_sound("creaking", 9);

    pfrm.load_overlay_texture("overlay_skyland_title");
    pfrm.load_sprite_texture("spritesheet_title_screen");

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, false, false);

    rng::critical_state = 2021;

    // if (pfrm.keyboard().pressed<Key::select>()) {
    //     flower_effect_ = true;
    // }
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
    Sprite sprite;
    sprite.set_priority(0);

    if (darken not_eq 0.f) {
        sprite.set_mix({ColorConstant::rich_black, u8(255 * darken)});
    }

    const Float sy = pfrm.screen().size().y;
    const Float sx = pfrm.screen().size().x;

    const auto view_origin = pfrm.screen().get_view().get_center();

    auto spr = [&](int t, Float x, Float y) {
        sprite.set_texture_index(t);
        sprite.set_position({x + view_origin.x, y + view_origin.y});
        pfrm.screen().draw(sprite);
    };

    sprite.set_size(Sprite::Size::w32_h32);
    spr(21, 0 - scroll, (sy - 16) + scroll / 2);
    spr(20, 0 - scroll, (sy - 48) + scroll / 2);
    spr(19, 0 - scroll, (sy - 80) + scroll / 2);

    spr(22, 32 - scroll, (sy - 16) + scroll / 2);
    spr(23, 64 - scroll, (sy - 16) + scroll / 2);


    spr(25, (sx - 32) + scroll, (sy - 32) + scroll / 2);
    spr(24, (sx - 32) + scroll, (sy - 64) + scroll / 2);
    spr(26, (sx - 64) + scroll, (sy - 32) + scroll / 2);
    spr(27, (sx - 96) + scroll, (sy - 16) + scroll / 2);


    spr(28, 0 - scroll, (sy - 112) + scroll / 4);
    spr(29, 0 - scroll, (sy - 144) + scroll / 4);

    spr(30, (sx - 32) + scroll, (sy - 124) + scroll / 4);

    // spr(32, (sx - 32) + scroll, 0 - scroll / 4);
    // spr(33, (sx - 32) + scroll, 32 - scroll / 4);
    // spr(34, (sx - 64) + scroll, 0 - scroll / 4);
}



void IntroCreditsScene::display(Platform& pfrm, App& app)
{
    if (not flower_effect_) {
        return;
    }

    if (not wait_ and not text_) {
        auto amount = smoothstep(milliseconds(0), milliseconds(1200), timer_);

        auto darken_amount =
            smoothstep(milliseconds(0), milliseconds(600), timer_);

        show_sunflowers(pfrm, 32 * amount, darken_amount);
    } else {
        auto amount = smoothstep(
            milliseconds(100), milliseconds(4000), flower_effect_timer_);

        show_sunflowers(pfrm, 64 * (1.f - amount), 0.f);
    }
}



} // namespace skyland
