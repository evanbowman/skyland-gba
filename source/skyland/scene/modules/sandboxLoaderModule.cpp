////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sandboxLoaderModule.hpp"
#include "script/listBuilder.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



SandboxLoaderModule::ParamBuffer SandboxLoaderModule::parameters_;



void prep_level();



const SandboxLoaderModule::ParameterInfo
    SandboxLoaderModule::param_info[decltype(parameters_)::capacity()] = {
        {SystemString::sandbox_coins, 1000, 1000, 100000000},
        {SystemString::sandbox_terrain_size, 1, 4, 13},
        {SystemString::sandbox_music, 1, 0, 1},
        {SystemString::sandbox_building_dependencies, 1, 0, 1},
        {SystemString::sandbox_weather, 1, 1, 8},
        {SystemString::sandbox_characters, 1, 1, 6}};



int SandboxLoaderModule::get_setting(u8 slot)
{
    if (slot >= parameters_.size()) {
        return 0;
    }
    return parameters_[slot];
}



void SandboxLoaderModule::update_parameter(u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += loadstr(param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;


    auto boolean_field_str = parameters_[line_num] ? SYSTR(yes) : SYSTR(no);

    auto int_text_len = integer_text_length(parameters_[line_num]);
    if (is_boolean_field) {
        int_text_len = utf8::len(boolean_field_str->c_str());
    }

    auto get_weather_str = [&] {
        return loadstr((SystemString)((int)SystemString::weather_clear +
                                      (parameters_[line_num] - 1)));
    };

    bool is_weather_field = false;
    if (param_info[line_num].name_ == SystemString::sandbox_weather) {
        is_weather_field = true;
        int_text_len = utf8::len(get_weather_str()->c_str());
    }

    for (u32 i = temp.length(); i < 28 - int_text_len - 2; ++i) {
        if (i % 2 == 0) {
            temp.push_back('.');
        } else {
            temp.push_back(' ');
        }
    }

    if (is_boolean_field) {
        temp += boolean_field_str->c_str();
    } else if (is_weather_field) {
        temp += get_weather_str()->c_str();
    } else {
        temp += stringify(parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());
}



void SandboxLoaderModule::enter(Scene& prev)
{
    APP.game_mode() = App::GameMode::sandbox;

    APP.camera()->reset();
    PLATFORM.screen().set_view({});

    PLATFORM.load_overlay_texture("overlay_challenges");

    PLATFORM_EXTENSION(vertical_parallax_enable, false);


    const StringBuffer<32> title = SYSTR(sandbox_title)->c_str();

    PLATFORM.load_overlay_texture("overlay_challenges");

    title_.emplace(

        title.c_str(),
        OverlayCoord{(u8)centered_text_margins(utf8::len(title.c_str())), 1});

    const StringBuffer<32> help = SYSTR(sandbox_prompt)->c_str();

    help_.emplace(

        help.c_str(),
        OverlayCoord{(u8)centered_text_margins(utf8::len(help.c_str())), 18});

    if (not parameters_.full()) {
        for (u32 i = 0; i < parameters_.capacity(); ++i) {
            parameters_.push_back(0);
        }

        // Defaults
        parameters_[0] = 500000;
        parameters_[1] = 4;
        parameters_[2] = 1;
        parameters_[3] = 0;
        parameters_[4] = 1;
        parameters_[5] = 2;
    } else {
        environment_init(parameters_[4]);
        PLATFORM.screen().set_shader(APP.environment().shader());

        auto new_ambiance = APP.environment().ambiance();
        if (not PLATFORM.speaker().is_music_playing(new_ambiance->c_str())) {
            PLATFORM.speaker().stream_music(new_ambiance->c_str(), 0);
        }
    }

    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(OverlayCoord{2, u8(4 + i * 2)});
    }

    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(i);
    }
}



void SandboxLoaderModule::exit(Scene& next)
{
    title_.reset();
    help_.reset();
    settings_text_.clear();


    if (not cancelled_) {

        environment_init(parameters_[4]);

        if (parameters_[2]) {
            PLATFORM.speaker().stream_music(APP.environment().music()->c_str(),
                                            0);
        } else {
            PLATFORM.speaker().stream_music(
                APP.environment().ambiance()->c_str(), 0);
        }

        lisp::ListBuilder list;
        for (auto& param : parameters_) {
            list.push_back(L_INT(param));
        }

        lisp::set_var("conf", list.result());

        APP.invoke_script("/scripts/sandbox/new.lisp");

        prep_level();

        show_island_exterior(&APP.player_island());
        show_island_exterior(APP.opponent_island());

        PLATFORM.load_overlay_texture("overlay");
        PLATFORM_EXTENSION(vertical_parallax_enable, true);

        PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

        APP.birds().clear();

        GenericBird::generate();
    }
}



void SandboxLoaderModule::display()
{
    Scene::display();

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position({2.0_fixed, Fixnum::from_integer(31.f + cursor_ * 16)});

    PLATFORM.screen().draw(spr);
}



ScenePtr SandboxLoaderModule::update(Time delta)
{
    APP.update_parallax(delta);

    APP.player().update(delta);

    if (APP.player().key_down(Key::action_1) or APP.player().tap_released()) {
        PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return make_scene<FadeInScene>();
    } else if (APP.player().key_down(Key::action_2)) {
        cancelled_ = true;
        PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return make_scene<TitleScreenScene>(3);
    }

    if (unveil_) {
        PLATFORM.screen().schedule_fade(
            0.6f, ColorConstant::rich_black, false, false);
    } else {
        unveil_ = true;
    }

    if (APP.player().key_pressed(Key::left)) {
        long_hold_time_[1] += delta;
    } else {
        long_hold_time_[1] = 0;
    }

    if (APP.player().key_pressed(Key::right)) {
        long_hold_time_[0] += delta;
    } else {
        long_hold_time_[0] = 0;
    }

    auto update_env = [&] {
        auto last_ambiance = APP.environment().ambiance();

        environment_init(parameters_[4]);
        PLATFORM_EXTENSION(vertical_parallax_enable, false);

        PLATFORM.screen().set_shader(APP.environment().shader());
        PLATFORM.screen().set_shader_argument(0);

        PLATFORM.screen().schedule_fade(
            0.7f, ColorConstant::rich_black, false, false);
        PLATFORM.screen().schedule_fade(
            0.6f, ColorConstant::rich_black, false, false);

        auto new_ambiance = APP.environment().ambiance();
        if (*last_ambiance not_eq new_ambiance->c_str()) {
            PLATFORM.speaker().stream_music(new_ambiance->c_str(), 0);
        }
    };


    if (APP.player().key_down(Key::right) or
        APP.player().key_held(Key::right, milliseconds(500))) {
        if (parameters_[cursor_] < param_info[cursor_].upper_limit_) {
            parameters_[cursor_] += param_info[cursor_].increment_;

            if (long_hold_time_[0] > milliseconds(2000)) {
                parameters_[cursor_] += param_info[cursor_].increment_ * 9;
            }

            if (cursor_ == 4) {
                update_env();
            }
        }
        update_parameter(cursor_);
        APP.player().key_held_reset(Key::right, milliseconds(80));
    }

    if (APP.player().key_down(Key::left) or
        APP.player().key_held(Key::left, milliseconds(500))) {
        parameters_[cursor_] -= param_info[cursor_].increment_;
        if (long_hold_time_[1] > milliseconds(2000)) {
            parameters_[cursor_] -= param_info[cursor_].increment_ * 9;
        }
        if (parameters_[cursor_] < param_info[cursor_].lower_limit_) {
            parameters_[cursor_] = param_info[cursor_].lower_limit_;
        }
        update_parameter(cursor_);
        APP.player().key_held_reset(Key::left, milliseconds(80));

        if (long_hold_time_[0] > milliseconds(2000)) {
            parameters_[cursor_] += param_info[cursor_].increment_ * 3;
        }

        if (cursor_ == 4) {
            update_env();
        }
    }

    if (APP.player().key_down(Key::down) and cursor_ < parameters_.size() - 1) {
        ++cursor_;
        PLATFORM.speaker().play_sound("click_wooden", 2);

    } else if (APP.player().key_down(Key::up) and cursor_ > 0) {
        --cursor_;
        PLATFORM.speaker().play_sound("click_wooden", 2);
    }


    return null_scene();
}



SandboxLoaderModule::Factory SandboxLoaderModule::factory_;



} // namespace skyland
