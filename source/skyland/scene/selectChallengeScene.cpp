////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "selectChallengeScene.hpp"
#include "achievementNotificationScene.hpp"
#include "fadeInScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



u8 SelectChallengeScene::page_ = 0;
u8 SelectChallengeScene::cursor_ = 0;



static const Float default_fade = 0.6f;



void SelectChallengeScene::enter(Scene& prev)
{
    PLATFORM.screen().set_shader(passthrough_shader);

    APP.swap_environment<weather::ClearSkies>();

    PLATFORM.load_overlay_texture("overlay_challenges");

    PLATFORM_EXTENSION(vertical_parallax_enable, false);

    APP.player_island().clear_rooms();
    APP.player_island().projectiles().clear();
    APP.effects().clear();

    challenges_ = APP.invoke_script("/scripts/challenges/challenge.lisp");

    const auto challenge_count = lisp::length(*challenges_);

    page_count_ = challenge_count / 5 + (challenge_count % 5 ? 1 : 0);

    show_options();

    for (int i = 0; i < 16; ++i) {
        for (int j = 0; j < 16; ++j) {
            PLATFORM.set_tile(Layer::map_0_ext, i, j, 0);
            PLATFORM.set_tile(Layer::map_1_ext, i, j, 0);
        }
    }

    PLATFORM.screen().set_view({});

    PLATFORM.screen().schedule_fade(
        default_fade, ColorConstant::rich_black, {}, false);

    if (not PLATFORM.speaker().is_music_playing("unaccompanied_wind")) {
        PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
    }
}



void SelectChallengeScene::show_options()
{
    PLATFORM.screen().clear();
    text_.clear();
    PLATFORM.screen().display();

    PLATFORM.fill_overlay(0);

    PLATFORM.set_tile(Layer::overlay, 1, 2, 90);
    PLATFORM.set_tile(Layer::overlay, 28, 2, 92);
    PLATFORM.set_tile(Layer::overlay, 1, 15, 94);
    PLATFORM.set_tile(Layer::overlay, 28, 15, 96);
    for (int x = 2; x < 28; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 2, 91);
        PLATFORM.set_tile(Layer::overlay, x, 15, 95);
    }
    for (int y = 3; y < 15; ++y) {
        PLATFORM.set_tile(Layer::overlay, 1, y, 93);
        PLATFORM.set_tile(Layer::overlay, 28, y, 97);
    }

    if (not challenges_) {
        return;
    }

    int index = 0;
    int start_index = page_ * 5;

    lisp::l_foreach(*challenges_, [&](lisp::Value* val) {
        if (val->type() not_eq lisp::Value::Type::cons) {
            PLATFORM.fatal("challenge list format invalid");
        }

        auto name = val->cons().car();
        if (name->type() not_eq lisp::Value::Type::string) {
            PLATFORM.fatal("challenge list format invalid");
        }

        bool completed = APP.gp_.challenge_flags_.get() & (1 << index);

        if (index++ < start_index) {
            return;
        }

        if (index > start_index + 5) {
            return;
        }

        text_.emplace_back(name->string().value(),
                           OverlayCoord{4, u8(4 + text_.size() * 2)});

        if (completed) {
            PLATFORM.set_tile(Layer::overlay,
                              text_.back().coord().x + text_.back().len() + 1,
                              text_.back().coord().y,
                              84);
        }
    });


    if (page_count_ > 1) {
        int margin = (calc_screen_tiles().x - page_count_ * 2) / 2;
        for (int i = 0; i < page_count_; ++i) {
            if (i == page_) {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 83);
            } else {
                PLATFORM.set_tile(Layer::overlay, margin + i * 2, 18, 82);
            }
        }
    }
}



void prep_level();



void SelectChallengeScene::exit(Scene& next)
{
    text_.clear();
    PLATFORM.fill_overlay(0);
    PLATFORM.load_overlay_texture("overlay");

    PLATFORM_EXTENSION(vertical_parallax_enable, true);

    PLATFORM.screen().fade(1.f);
}



void SelectChallengeScene::display()
{
    if (state_ not_eq State::idle) {
        return;
    }
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    Vec2<Fixnum> origin;

    auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
                            std::numeric_limits<s16>::max();

    origin.x += Fixnum(16 + ambient_movement);
    origin.y += Fixnum(32 + cursor_ * 16 - 1);

    cursor.set_position(origin);

    PLATFORM.screen().draw(cursor);
}



ScenePtr SelectChallengeScene::update(Time delta)
{
    if (exit_) {
        page_ = 0;
        cursor_ = 0;
        return make_scene<TitleScreenScene>(2);
    }

    timer_ += delta;

    switch (state_) {
    case State::fade_in:
        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update();
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(achievement);

                APP.player_island().show_flag(false);

                PLATFORM.screen().fade(1.f);

                auto next = make_deferred_scene<SelectChallengeScene>();
                return make_scene<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }
        state_ = State::idle;
        break;

    case State::idle: {
        if (not challenges_) {
            return null_scene();
        }

        if (APP.player().key_down(Key::down)) {
            if ((u32)cursor_ < text_.size() - 1) {
                cursor_++;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }

        if (APP.player().key_down(Key::up)) {
            if (cursor_) {
                cursor_--;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }

        if (APP.player().key_down(Key::right)) {
            if (page_ < page_count_ - 1) {
                ++page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::left)) {
            if (page_ > 0) {
                --page_;
                show_options();
                if ((u32)cursor_ >= text_.size()) {
                    cursor_ = text_.size() - 1;
                }
            }
        }

        if (APP.player().key_down(Key::action_1)) {
            state_ = State::fade_out;
            timer_ = 0;
            text_.clear();
            PLATFORM.fill_overlay(0);
        } else if (APP.player().key_down(Key::action_2)) {
            text_.clear();
            PLATFORM.fill_overlay(0);
            exit_ = true;
        }
        break;
    }

    case State::fade_out: {
        constexpr auto fade_duration = milliseconds(800);
        if (timer_ > fade_duration) {
            APP.camera()->reset();
            PLATFORM.screen().fade(
                1.f, ColorConstant::rich_black, {}, true, true);
            auto index = page_ * 5 + cursor_;
            auto choice = lisp::get_list(*challenges_, index);

            auto file_name = choice->cons().cdr();
            if (file_name->type() not_eq lisp::Value::Type::string) {
                PLATFORM.fatal("challenge list format invalid");
            }

            APP.set_coins(0);

            StringBuffer<100> path("/scripts/");
            path += file_name->string().value();
            APP.invoke_script(path.c_str());

            prep_level();

            show_island_exterior(&APP.player_island());

            if (not PLATFORM.speaker().is_music_playing(
                    APP.environment().music()->c_str())) {
                PLATFORM.speaker().stream_music(
                    APP.environment().music()->c_str(), 0);
            }

            return make_scene<FadeInScene>();

        } else {
            const auto amount =
                default_fade +
                (1.f - default_fade) * smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().fade(amount);
        }
        break;
    }
    }

    APP.update_parallax(delta);

    return null_scene();
}



} // namespace skyland
