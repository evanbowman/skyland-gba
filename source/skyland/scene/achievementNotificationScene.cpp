////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "achievementNotificationScene.hpp"
#include "readyScene.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr AchievementNotificationScene::update(Time delta)
{
    // NOTE: do not call WorldScene::update, the game should be considered
    // paused.

    constexpr auto fade_duration = milliseconds(300);

    timer_ += delta;

    switch (state_) {
    case State::fade_in: {
        if (skip_fade_ or timer_ > fade_duration) {
            PLATFORM.screen().schedule_fade(0.5f);
            timer_ = 0;
            PLATFORM.speaker().play_sound("click_digital_1", 1);
            state_ = State::animate_box_sweep;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(0.5f * amount);
        }
        break;
    }

    case State::animate_box_sweep: {
        static const auto sweep_duration = milliseconds(100);

        const auto st = calc_screen_tiles();

        if (timer_ > sweep_duration) {
            timer_ = 0;
            state_ = State::wait;

            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    PLATFORM.set_tile(Layer::overlay, x, y, 112);
                }
            }

            for (int x = 4; x < st.x - 4; ++x) {
                PLATFORM.set_tile(Layer::overlay, x, 8, 377);
            }

            PLATFORM.set_tile(Layer::overlay, st.x - 5, 4, 378);

            auto mt = load_metaclass(achievements::reward(achievement_));
            if (not mt) {
                Platform::fatal("missing metaclass for achievement");
            }

            const auto banner_color = Text::OptColors{
                {ColorConstant::rich_black, custom_color(0xead873)}};


            achievement_text_.emplace(OverlayCoord{3, 4});
            achievement_text_->append(" ", banner_color);
            achievement_text_->append(SYSTR(achievement_msg_title)->c_str(),
                                      banner_color);


            PLATFORM.set_tile(
                Layer::overlay, 3 + achievement_text_->len(), 4, 482);
            for (int x = 0; x < achievement_text_->len() + 1; ++x) {
                PLATFORM.set_tile(Layer::overlay, 3 + x, 3, 478);
            }
            PLATFORM.set_tile(Layer::overlay, 2, 4, 480);
            PLATFORM.set_tile(Layer::overlay, 2, 5, 481);
            PLATFORM.set_tile(Layer::overlay, 2, 3, 479);


            achievement_name_.emplace(OverlayCoord{4, 6});
            achievement_name_->assign(
                loadstr(achievements::name(achievement_))->c_str());

            unlocked_text_.emplace(OverlayCoord{4, 9});
            unlocked_text_->assign(SYSTR(achievement_msg_unlocked)->c_str());

            StringBuffer<17> temp;

            item_name_.emplace(OverlayCoord{9, 11});

            make_format(temp, "%", (*mt)->ui_name()->c_str());

            item_name_->assign(temp.c_str());

            temp.clear();

            make_format(temp,
                        "%@ %` %hp",
                        (*mt)->cost(),
                        (*mt)->consumes_power(),
                        (*mt)->full_health());

            item_details_.emplace(OverlayCoord{9, 13});
            item_details_->assign(temp.c_str());

            auto icon = (*mt)->unsel_icon();
            draw_image(181, 4, 11, 4, 4, Layer::overlay);
            PLATFORM.load_overlay_chunk(181, icon, 16);

        } else {

            const int total = st.y - 7;
            const int progress =
                total * smoothstep(0.f, sweep_duration, timer_);

            for (int x = 3; x < st.x - 3; ++x) {
                for (int y = 3; y < st.y - 4; ++y) {
                    if (y <= progress + 3) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 112);
                    }
                }
            }
        }

        break;
    }

    case State::wait:
        if (timer_ > seconds(2) and (APP.player().key_down(Key::action_1) or
                                     APP.player().key_down(Key::action_2))) {

            timer_ = 0;
            state_ = State::fade_out;

            achievement_name_.reset();
            achievement_text_.reset();

            PLATFORM.fill_overlay(0);
        }
        break;

    case State::fade_out:
        if (skip_fade_) {
            return next_scene_();
        }
        if (timer_ > fade_duration) {
            PLATFORM.screen().fade(0.f);
            return next_scene_();
        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(0.5f * amount);
        }
        break;
    }

    return null_scene();
}



void AchievementNotificationScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    // PLATFORM.screen().schedule_fade(0.5f);
}



void AchievementNotificationScene::exit(Scene& next)
{
    WorldScene::exit(next);
}



} // namespace skyland
