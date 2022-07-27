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


#include "skylandForever.hpp"
#include "skyland/player/opponent/procgenEnemyAI.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void prep_level(Platform& pfrm, App& app);
void environment_init(App& app, int type);



const SkylandForever::ParameterInfo
    SkylandForever::param_info[decltype(parameters_)::capacity()] = {
        {SystemString::sf_difficulty, 1, 0, 2},
        {SystemString::sandbox_weather, 1, 1, 3},
};



static const u8 settings_start = 10;



void SkylandForever::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_challenges");
    pfrm.system_call("v-parallax", (void*)false);


    app.level_coins_spent() = 0;


    msg_.emplace(pfrm);
    msg_->assign(SYSTR(sf_description)->c_str(),
                 OverlayCoord{1, 4},
                 OverlayCoord{28, 6});


    app.game_mode() = App::GameMode::skyland_forever;

    parameters_.push_back(1);
    parameters_.push_back(1);

    parameters_[0] = (int)app.gp_.difficulty_;

    environment_init(app, parameters_[1]);
    pfrm.screen().set_shader(app.environment().shader(app));

    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(
            pfrm, OverlayCoord{2, u8(settings_start + i * 2)});
    }

    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(pfrm, i);
    }

    const auto help = SYSTR(sf_hint);

    help_.emplace(
        pfrm,
        help->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(help->c_str())),
                     18});

    const auto title = SYSTR(sf_title);

    title_.emplace(
        pfrm,
        title->c_str(),
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(title->c_str())),
                     1});
}



void SkylandForever::exit(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);

    help_.reset();
    title_.reset();
    settings_text_.clear();
    msg_.reset();

    environment_init(app, parameters_[1]);

    pfrm.load_overlay_texture("overlay");
    pfrm.system_call("v-parallax", (void*)true);

    init(pfrm, app, parameters_[0], rng::critical_state);

    pfrm.load_overlay_texture("overlay");
    pfrm.system_call("v-parallax", (void*)true);

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
}



void SkylandForever::init(Platform& pfrm,
                          App& app,
                          u8 difficulty,
                          rng::LinearGenerator seed)
{
    app.set_coins(pfrm, 0);

    switch (difficulty) {
    case 0:
        app.gp_.difficulty_ = GlobalPersistentData::Difficulty::beginner;
        app.invoke_script(pfrm, "/scripts/config/forever/easy.lisp");
        break;

    case 1:
        app.gp_.difficulty_ = GlobalPersistentData::Difficulty::experienced;
        app.invoke_script(pfrm, "/scripts/config/forever/normal.lisp");
        break;

    case 2:
        app.gp_.difficulty_ = GlobalPersistentData::Difficulty::expert;
        app.invoke_script(pfrm, "/scripts/config/forever/hard.lisp");
        break;
    }

    app.player_island().init_terrain(pfrm, 4);

    app.persistent_data().score_.set(0);

    pfrm.speaker().play_music(app.environment().music(), 0);

    app.invoke_script(pfrm, "/scripts/event/skyland_forever.lisp");

    prep_level(pfrm, app);
    app.player_island().set_position({10, 374});

    app.reset_opponent_island(pfrm);
    app.swap_opponent<ProcgenEnemyAI>(seed, difficulty);


    show_island_exterior(pfrm, app, &app.player_island());
    show_island_exterior(pfrm, app, app.opponent_island());
}



ScenePtr<Scene>
SkylandForever::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.update_parallax(delta);

    app.player().update(pfrm, app, delta);

    if (unveil_) {
        pfrm.screen().schedule_fade(
            0.6f, ColorConstant::rich_black, false, false);
    } else {
        unveil_ = true;
    }

    auto update_env = [&] {
        environment_init(app, parameters_[1]);

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
        }
        update_parameter(pfrm, cursor_);
        app.player().key_held_reset(Key::right, milliseconds(80));
        if (cursor_ == 1) {
            update_env();
        }
    }

    if (app.player().key_down(pfrm, Key::left) or
        app.player().key_held(Key::left, milliseconds(500))) {
        parameters_[cursor_] -= param_info[cursor_].increment_;

        if (parameters_[cursor_] < param_info[cursor_].lower_limit_) {
            parameters_[cursor_] = param_info[cursor_].lower_limit_;
        }
        update_parameter(pfrm, cursor_);
        app.player().key_held_reset(Key::left, milliseconds(80));
        if (cursor_ == 1) {
            update_env();
        }
    }

    if (app.player().key_down(pfrm, Key::down) and
        cursor_ < parameters_.size() - 1) {
        ++cursor_;

    } else if (app.player().key_down(pfrm, Key::up) and cursor_ > 0) {
        --cursor_;
    }

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().tap_released(pfrm)) {
        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return scene_pool::alloc<FadeInScene>();
    } else if (app.player().key_down(pfrm, Key::action_2)) {
        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return scene_pool::alloc<TitleScreenScene>(3);
    }

    return null_scene();
}



void SkylandForever::display(Platform& pfrm, App& app)
{
    Scene::display(pfrm, app);

    Sprite spr;
    spr.set_size(Sprite::Size::w16_h32);
    spr.set_texture_index(59);
    spr.set_position({2, ((settings_start * 8) - 1.f) + cursor_ * 16});

    pfrm.screen().draw(spr);
}



void SkylandForever::update_parameter(Platform& pfrm, u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += loadstr(pfrm, param_info[line_num].name_)->c_str();
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;

    auto int_text_len = integer_text_length(parameters_[line_num]);

    StringBuffer<48> text = "";

    auto boolean_field_str = parameters_[line_num] ? SYSTR(yes) : SYSTR(no);

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
    } else {
        temp += stringify(parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());
}



SkylandForever::Factory SkylandForever::factory_;



} // namespace skyland
