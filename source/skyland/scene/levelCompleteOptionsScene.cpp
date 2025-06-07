////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "levelCompleteOptionsScene.hpp"
#include "levelExitScene.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



static const Float partial_fade_amt = 0.76f;



void update_confetti(ConfettiBuffer& confetti, Time delta);



void LevelCompleteOptionsScene::show_cursor()
{
    int tiles[2] = {0, 0};
    tiles[cursor_] = 436;

    PLATFORM.set_tile(Layer::overlay,
                      options_[0].coord().x - 2,
                      options_[0].coord().y,
                      tiles[0]);
    PLATFORM.set_tile(Layer::overlay,
                      options_[1].coord().x - 2,
                      options_[1].coord().y,
                      tiles[1]);
}



ScenePtr LevelCompleteOptionsScene::update(Time delta)
{
    WorldScene::update(delta);

    if (confetti_ and *confetti_) {
        update_confetti(**confetti_, delta);
    }

    switch (state_) {
    case State::select:
        timer_ += delta;
        if (APP.player().key_down(Key::action_1)) {
            timer_ = 0;
            switch (cursor_) {
            case 0:
                state_ = State::fade_out;
                break;

            case 1:
                state_ = State::fade_resume;
                break;

            case 2:
                break;
            }
            options_.clear();
            PLATFORM.fill_overlay(0);
        } else if (APP.player().key_down(Key::action_2)) {
            state_ = State::fade_resume;
            options_.clear();
            PLATFORM.fill_overlay(0);
            timer_ = 0;
        } else if (APP.player().key_down(Key::up) and cursor_ > 0) {
            --cursor_;
            show_cursor();

        } else if (APP.player().key_down(Key::down) and cursor_ < 1) {
            ++cursor_;
            show_cursor();
        }
        break;

    case State::fade_resume: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(400);
        if (timer_ > fade_duration) {
            PLATFORM.screen().set_shader(APP.environment().shader());
            PLATFORM.load_overlay_texture("overlay");
            PLATFORM.screen().schedule_fade(0.1f); // palette bugfix
            PLATFORM.screen().schedule_fade(0.f);
            PLATFORM.screen().pixelate(0, false);
            APP.reset_opponent_island();
            return make_scene<ReadyScene>();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().pixelate(amount * 28, false);
            PLATFORM.screen().schedule_fade(partial_fade_amt -
                                            amount * partial_fade_amt);
        }
        break;
    }

    case State::fade_out: {
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            PLATFORM.load_overlay_texture("overlay");
            hide_translucence();
            return make_scene<LevelExitScene<ZoneImageScene>>();
        } else {
            const auto amount =
                partial_fade_amt + (1.f - partial_fade_amt) *
                                       smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(amount);
        }
        break;
    }

    case State::fade_in:
        timer_ += delta;
        constexpr auto fade_duration = milliseconds(500);
        if (timer_ > fade_duration) {
            timer_ = 0;
            state_ = State::select;
            show_cursor();
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(amount * partial_fade_amt);
            PLATFORM.screen().pixelate(amount * 28, false);
        }
        break;
    }

    return null_scene();
}



void LevelCompleteOptionsScene::display()
{
    WorldScene::display();

    if (confetti_ and *confetti_) {
        for (auto& c : **confetti_) {
            Sprite spr_;
            spr_.set_priority(0);
            spr_.set_position({Fixnum(c.x_), Fixnum(c.y_)});
            spr_.set_size(Sprite::Size::w8_h8);
            spr_.set_mix({[&] {
                              switch (c.clr_) {
                              default:
                              case 0:
                                  return ColorConstant::spanish_crimson;
                              case 1:
                                  return custom_color(0xbdef84);
                              case 2:
                                  return custom_color(0x006bff);
                              }
                          }(),
                          255});
            const auto off = c.img_ - 102;
            spr_.set_texture_index(102 * 8 + off + c.anim_ * 4);
            PLATFORM.screen().draw(spr_);
        }
    }
}



void LevelCompleteOptionsScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    PLATFORM.load_overlay_texture("overlay_island_destroyed");

    auto st = calc_screen_tiles();

    StringBuffer<32> resume = SYSTR(start_menu_continue_building)->c_str();
    StringBuffer<32> sky_map = SYSTR(start_menu_sky_map)->c_str();

    options_.emplace_back(

        sky_map.c_str(),
        OverlayCoord{u8((st.x - utf8::len(sky_map.c_str())) / 2), 5});

    options_.emplace_back(

        resume.c_str(),
        OverlayCoord{u8((st.x - utf8::len(resume.c_str())) / 2), 7});

    if (state_ not_eq State::fade_in) {
        PLATFORM.screen().pixelate(partial_fade_amt * 28, false);
        show_cursor();
    }
}



void LevelCompleteOptionsScene::exit(Scene& next)
{
    WorldScene::exit(next);

    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.screen().pixelate(0.f);
}



} // namespace skyland
