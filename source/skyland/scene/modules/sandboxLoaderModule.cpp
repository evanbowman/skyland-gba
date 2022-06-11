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


#include "sandboxLoaderModule.hpp"
#include "script/listBuilder.hpp"
#include "skyland/entity/birds/genericBird.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/storm.hpp"
#include "skyland/weather/typhoon.hpp"



namespace skyland
{



SandboxLoaderModule::ParamBuffer SandboxLoaderModule::parameters_;



void prep_level(Platform& pfrm, App& app);



const SandboxLoaderModule::ParameterInfo
    SandboxLoaderModule::param_info[decltype(parameters_)::capacity()] = {
        {SystemString::sandbox_coins, 1000, 1000, 100000000},
        {SystemString::sandbox_terrain_size, 1, 4, 13},
        {SystemString::sandbox_music, 1, 0, 1},
        {SystemString::sandbox_building_dependencies, 1, 0, 1},
        {SystemString::sandbox_weather, 1, 1, 3},
        {SystemString::sandbox_characters, 1, 1, 6}};



int SandboxLoaderModule::get_setting(u8 slot)
{
    if (slot >= parameters_.size()) {
        return 0;
    }
    return parameters_[slot];
}



void environment_init(App& app, int type)
{
    switch (type) {
    case 1:
        app.swap_environment<weather::ClearSkies>();
        break;

    case 2:
        app.swap_environment<weather::Storm>();
        break;

    case 3:
        app.swap_environment<weather::Typhoon>();
        break;
    }
}



void SandboxLoaderModule::update_parameter(Platform& pfrm, u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += loadstr(pfrm, param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;


    auto boolean_field_str = parameters_[line_num] ? SYSTR(yes) : SYSTR(no);

    auto int_text_len = integer_text_length(parameters_[line_num]);
    if (is_boolean_field) {
        int_text_len = utf8::len(boolean_field_str->c_str());
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
    } else {
        temp += stringify(parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());
}



void SandboxLoaderModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    app.game_mode() = App::GameMode::sandbox;

    app.camera()->reset();
    pfrm.screen().set_view({});

    pfrm.load_overlay_texture("overlay_challenges");

    pfrm.system_call("v-parallax", (void*)false);


    const StringBuffer<32> title = SYSTR(sandbox_title)->c_str();

    pfrm.load_overlay_texture("overlay_challenges");

    title_.emplace(
        pfrm,
        title.c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(title.c_str())),
                     1});

    const StringBuffer<32> help = SYSTR(sandbox_prompt)->c_str();

    help_.emplace(
        pfrm,
        help.c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, utf8::len(help.c_str())),
                     18});

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
        environment_init(app, parameters_[4]);
        pfrm.screen().set_shader(app.environment().shader(app));
    }

    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(pfrm, OverlayCoord{2, u8(4 + i * 2)});
    }

    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(pfrm, i);
    }
}



void SandboxLoaderModule::exit(Platform& pfrm, App& app, Scene& next)
{
    title_.reset();
    help_.reset();
    settings_text_.clear();


    if (not cancelled_) {

        environment_init(app, parameters_[4]);

        if (parameters_[2]) {
            pfrm.speaker().play_music(app.environment().music(), 0);
        }

        lisp::ListBuilder list;
        for (auto& param : parameters_) {
            list.push_back(L_INT(param));
        }

        lisp::set_var("conf", list.result());

        app.invoke_script(pfrm, "/scripts/sandbox/new.lisp");

        prep_level(pfrm, app);

        show_island_exterior(pfrm, app, &app.player_island());
        show_island_exterior(pfrm, app, app.opponent_island());

        pfrm.load_overlay_texture("overlay");
        pfrm.system_call("v-parallax", (void*)true);

        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

        app.birds().clear();
        GenericBird::generate(pfrm, app);
    }
}



void SandboxLoaderModule::display(Platform& pfrm, App& app)
{
    Scene::display(pfrm, app);

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position({2, 31.f + cursor_ * 16});

    pfrm.screen().draw(spr);
}



ScenePtr<Scene>
SandboxLoaderModule::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    app.player().update(pfrm, app, delta);

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().tap_released(pfrm)) {
        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return scene_pool::alloc<FadeInScene>();
    } else if (app.player().key_down(pfrm, Key::action_2)) {
        cancelled_ = true;
        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    if (unveil_) {
        pfrm.screen().schedule_fade(
            0.6f, ColorConstant::rich_black, false, false);
    } else {
        unveil_ = true;
    }

    if (app.player().key_pressed(pfrm, Key::left)) {
        long_hold_time_[1] += delta;
    } else {
        long_hold_time_[1] = 0;
    }

    if (app.player().key_pressed(pfrm, Key::right)) {
        long_hold_time_[0] += delta;
    } else {
        long_hold_time_[0] = 0;
    }

    auto update_env = [&] {
        environment_init(app, parameters_[4]);

        pfrm.screen().set_shader(app.environment().shader(app));
        pfrm.screen().set_shader_argument(0);

        pfrm.screen().schedule_fade(
            0.7f, ColorConstant::rich_black, false, false);
        pfrm.screen().schedule_fade(
            0.6f, ColorConstant::rich_black, false, false);
    };


    if (app.player().key_down(pfrm, Key::right) or
        app.player().key_held(Key::right, milliseconds(500))) {
        if (parameters_[cursor_] < param_info[cursor_].upper_limit_) {
            parameters_[cursor_] += param_info[cursor_].increment_;

            if (long_hold_time_[0] > milliseconds(2000)) {
                parameters_[cursor_] += param_info[cursor_].increment_ * 9;
            }

            if (cursor_ == 4) {
                update_env();
            }
        }
        update_parameter(pfrm, cursor_);
        app.player().key_held_reset(Key::right, milliseconds(80));
    }

    if (app.player().key_down(pfrm, Key::left) or
        app.player().key_held(Key::left, milliseconds(500))) {
        parameters_[cursor_] -= param_info[cursor_].increment_;
        if (long_hold_time_[1] > milliseconds(2000)) {
            parameters_[cursor_] -= param_info[cursor_].increment_ * 9;
        }
        if (parameters_[cursor_] < param_info[cursor_].lower_limit_) {
            parameters_[cursor_] = param_info[cursor_].lower_limit_;
        }
        update_parameter(pfrm, cursor_);
        app.player().key_held_reset(Key::left, milliseconds(80));

        if (long_hold_time_[0] > milliseconds(2000)) {
            parameters_[cursor_] += param_info[cursor_].increment_ * 3;
        }

        if (cursor_ == 4) {
            update_env();
        }
    }

    if (app.player().key_down(pfrm, Key::down) and
        cursor_ < parameters_.size() - 1) {
        ++cursor_;
        pfrm.speaker().play_sound("click_wooden", 2);

    } else if (app.player().key_down(pfrm, Key::up) and cursor_ > 0) {
        --cursor_;
        pfrm.speaker().play_sound("click_wooden", 2);
    }


    return null_scene();
}



SandboxLoaderModule::Factory SandboxLoaderModule::factory_;



} // namespace skyland
