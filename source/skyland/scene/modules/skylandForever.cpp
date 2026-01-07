////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "skylandForever.hpp"
#include "skyland/player/opponent/procgenEnemyAI.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void prep_level();



const SkylandForever::ParameterInfo
    SkylandForever::param_info[decltype(parameters_)::capacity()] = {
        {SystemString::sf_difficulty, 1, 0, 2},
        {SystemString::sandbox_weather, 1, 1, 8},
};



static const u8 settings_start = 10;



void SkylandForever::enter(Scene& prev)
{
    PLATFORM.load_overlay_texture("overlay_challenges");
    PLATFORM_EXTENSION(vertical_parallax_enable, false);


    APP.level_coins_spent() = 0;


    msg_.emplace();
    msg_->assign(SYSTR(sf_description)->c_str(),
                 OverlayCoord{1, 4},
                 OverlayCoord{28, 6});


    APP.game_mode() = App::GameMode::skyland_forever;

    parameters_.push_back(1);
    parameters_.push_back(1);

    parameters_[0] = (int)APP.gp_.difficulty_;

    environment_init(parameters_[1]);
    PLATFORM.screen().set_shader(APP.environment().shader());

    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(
            OverlayCoord{2, u8(settings_start + i * 2)});
    }

    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(i);
    }

    const auto help = SYSTR(sf_hint);

    help_.emplace(

        help->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(help->c_str())), 18});

    const auto title = SYSTR(sf_title);

    title_.emplace(

        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(strlen(title->c_str())), 1});
}



void SkylandForever::exit(Scene& prev)
{
    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    help_.reset();
    title_.reset();
    settings_text_.clear();
    msg_.reset();

    environment_init(parameters_[1]);

    PLATFORM.load_overlay_texture("overlay");
    PLATFORM_EXTENSION(vertical_parallax_enable, true);

    init(parameters_[0], rng::critical_state);

    PLATFORM.load_overlay_texture("overlay");
    PLATFORM_EXTENSION(vertical_parallax_enable, true);

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
}



void SkylandForever::init(u8 difficulty, rng::LinearGenerator seed)
{
    APP.set_coins(0);

    switch (difficulty) {
    case 0:
        APP.gp_.difficulty_ = GlobalPersistentData::Difficulty::beginner;
        APP.invoke_script("/scripts/config/forever/easy.lisp");
        break;

    case 1:
        APP.gp_.difficulty_ = GlobalPersistentData::Difficulty::experienced;
        APP.invoke_script("/scripts/config/forever/normal.lisp");
        break;

    case 2:
        APP.gp_.difficulty_ = GlobalPersistentData::Difficulty::expert;
        APP.invoke_script("/scripts/config/forever/hard.lisp");
        break;
    }

    APP.persistent_data().total_seconds_.set(0);

    APP.player_island().init_terrain(4);

    APP.persistent_data().score_.set(0);

    PLATFORM.speaker().stream_music(APP.environment().music()->c_str(), 0);

    APP.invoke_script("/scripts/event/skyland_forever.lisp");

    prep_level();
    APP.player_island().set_position(
        {Fixnum::from_integer(10), Fixnum::from_integer(374)});

    APP.reset_opponent_island();
    APP.swap_opponent<ProcgenEnemyAI>(seed, difficulty);


    show_island_exterior(&APP.player_island());
    show_island_exterior(APP.opponent_island());
}



ScenePtr SkylandForever::update(Time delta)
{
    APP.update_parallax(delta);

    APP.player().update(delta);

    if (unveil_) {
        PLATFORM.screen().schedule_fade(
            0.6f, {ColorConstant::rich_black, false, false});
    } else {
        unveil_ = true;
    }

    auto update_env = [&] {
        auto last_ambiance = APP.environment().ambiance();

        environment_init(parameters_[1]);
        PLATFORM_EXTENSION(vertical_parallax_enable, false);

        PLATFORM.screen().set_shader(APP.environment().shader());
        PLATFORM.screen().set_shader_argument(0);

        PLATFORM.screen().schedule_fade(
            0.7f, {ColorConstant::rich_black, false, false});
        PLATFORM.screen().schedule_fade(
            0.6f, {ColorConstant::rich_black, false, false});

        auto new_ambiance = APP.environment().ambiance();
        if (*last_ambiance not_eq new_ambiance->c_str()) {
            PLATFORM.speaker().stream_music(new_ambiance->c_str(), 0);
        }
    };

    if (APP.player().key_down(Key::right) or
        APP.player().key_held(Key::right, milliseconds(500))) {
        if (parameters_[cursor_] < param_info[cursor_].upper_limit_) {
            parameters_[cursor_] += param_info[cursor_].increment_;
        }
        update_parameter(cursor_);
        APP.player().key_held_reset(Key::right, milliseconds(80));
        if (cursor_ == 1) {
            update_env();
        }
    }

    if (APP.player().key_down(Key::left) or
        APP.player().key_held(Key::left, milliseconds(500))) {
        parameters_[cursor_] -= param_info[cursor_].increment_;

        if (parameters_[cursor_] < param_info[cursor_].lower_limit_) {
            parameters_[cursor_] = param_info[cursor_].lower_limit_;
        }
        update_parameter(cursor_);
        APP.player().key_held_reset(Key::left, milliseconds(80));
        if (cursor_ == 1) {
            update_env();
        }
    }

    if (APP.player().key_down(Key::down) and cursor_ < parameters_.size() - 1) {
        ++cursor_;

    } else if (APP.player().key_down(Key::up) and cursor_ > 0) {
        --cursor_;
    }

    if (APP.player().key_down(Key::action_1) or APP.player().tap_released()) {
        PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return make_scene<FadeInScene>();
    } else if (APP.player().key_down(Key::action_2)) {
        PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return make_scene<TitleScreenScene>(3);
    }

    return null_scene();
}



void SkylandForever::display()
{
    Scene::display();

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_priority(0);
    spr.set_position(
        {2.0_fixed,
         Fixnum::from_integer(((settings_start * 8) - 1.f) + cursor_ * 16)});

    PLATFORM.screen().draw(spr);
}



void SkylandForever::update_parameter(u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += loadstr(param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;

    auto int_text_len = integer_text_length(parameters_[line_num]);

    StringBuffer<48> text = "";

    auto boolean_field_str = parameters_[line_num] ? SYSTR(yes) : SYSTR(no);

    auto get_weather_str = [&] {
        return loadstr((SystemString)((int)SystemString::weather_clear +
                                      (parameters_[line_num] - 1)));
    };

    bool is_weather_field = false;
    if (param_info[line_num].name_ == SystemString::sandbox_weather) {
        is_weather_field = true;
        int_text_len = utf8::len(get_weather_str()->c_str());
    }

    if (line_num == 0) {
        switch (parameters_[line_num]) {
        case 0:
            text = SYSTR(sf_casual)->c_str();
            break;

        case 1:
            text = SYSTR(sf_normal)->c_str();
            ;
            break;

        case 2:
            text = SYSTR(sf_hard)->c_str();
            ;
            break;
        }
        int_text_len = utf8::len(text.c_str());
    } else if (is_boolean_field) {
        if (is_boolean_field) {
            int_text_len = utf8::len(boolean_field_str->c_str());
        }
    }

    for (u32 i = temp.length(); i < 28 - int_text_len - 2; ++i) {
        if (i % 2 == 0) {
            temp.push_back('.');
        } else {
            temp.push_back(' ');
        }
    }

    if (line_num == 0) {
        temp += text;
    } else if (is_boolean_field) {
        temp += boolean_field_str->c_str();
    } else if (is_weather_field) {
        temp += get_weather_str()->c_str();
    } else {
        temp += stringify(parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());
}



SkylandForever::Factory SkylandForever::factory_;



} // namespace skyland
