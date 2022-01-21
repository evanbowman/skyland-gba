#include "sandboxLoaderModule.hpp"
#include "localization.hpp"
#include "skyland/scene/fadeInScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



SandboxLoaderModule::ParamBuffer SandboxLoaderModule::parameters_;



void prep_level(Platform& pfrm, App& app);



const SandboxLoaderModule::ParameterInfo
    SandboxLoaderModule::param_info[decltype(parameters_)::capacity()] = {
        {"coins", 1000, 1000, 100000000},
        {"terrain size", 1, 4, 13},
};



void SandboxLoaderModule::update_parameter(u8 line_num)
{
    if (line_num >= parameters_.capacity()) {
        return;
    }

    StringBuffer<28> temp;
    temp += param_info[line_num].name_;
    temp += " ";

    const bool is_boolean_field = param_info[line_num].lower_limit_ == 0 and
                                  param_info[line_num].upper_limit_ == 1;

    auto int_text_len = integer_text_length(parameters_[line_num]);
    if (is_boolean_field) {
        if (parameters_[line_num]) {
            int_text_len = str_len("yes");
        } else {
            int_text_len = str_len("no");
        }
    }

    for (u32 i = temp.length(); i < 28 - int_text_len - 2; ++i) {
        if (i % 2 == 0) {
            temp.push_back('.');
        } else {
            temp.push_back(' ');
        }
    }

    if (is_boolean_field) {
        temp += parameters_[line_num] ? "yes" : "no";
    } else {
        temp += to_string<10>(parameters_[line_num]);
    }

    settings_text_[line_num].assign(temp.c_str());
}



void SandboxLoaderModule::enter(Platform& pfrm, App& app, Scene& prev)
{
    app.game_mode() = App::GameMode::sandbox;

    app.camera().reset();
    pfrm.screen().set_view({});

    pfrm.load_overlay_texture("overlay_challenges");

    pfrm.system_call("v-parallax", (void*)false);


    const char* title = "Sandbox Settings";

    pfrm.load_overlay_texture("overlay_challenges");

    title_.emplace(
        pfrm,
        title,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(title)), 1});

    const char* help = "Press A to begin";

    help_.emplace(
        pfrm,
        help,
        OverlayCoord{(u8)centered_text_margins(pfrm, str_len(title)), 18});

    if (not parameters_.full()) {
        for (u32 i = 0; i < parameters_.capacity(); ++i) {
            parameters_.push_back(0);
        }

        // Defaults
        parameters_[0] = 500000;
        parameters_[1] = 4;
    }

    for (u32 i = 0; i < settings_text_.capacity(); ++i) {
        settings_text_.emplace_back(pfrm, OverlayCoord{2, u8(4 + i * 2)});
    }

    for (u32 i = 0; i < parameters_.capacity(); ++i) {
        update_parameter(i);
    }
}



void SandboxLoaderModule::exit(Platform& pfrm, App& app, Scene& prev)
{
    title_.reset();
    help_.reset();
    settings_text_.clear();

    app.set_coins(pfrm, parameters_[0]);
    app.player_island().init_terrain(pfrm, parameters_[1]);

    pfrm.speaker().play_music("sb_solecism", 0);

    app.invoke_script(pfrm, "/scripts/sandbox.lisp");
    pfrm.delta_clock().reset();

    prep_level(pfrm, app);
    pfrm.load_tile0_texture("tilesheet");
    pfrm.load_tile1_texture("tilesheet_enemy_0");
    app.player_island().render_exterior(pfrm, app);

    vram_write_flag(pfrm, app.gp_.flag_img_);

    pfrm.load_overlay_texture("overlay");
    pfrm.system_call("v-parallax", (void*)true);

    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
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

    if (app.player().key_down(pfrm, Key::action_1)) {
        pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
        return scene_pool::alloc<FadeInScene>();
    }

    if (unveil_) {
        pfrm.screen().fade(0.6f, ColorConstant::rich_black, {}, false, false);
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


    if (app.player().key_down(pfrm, Key::right) or
        app.player().key_held(Key::right, milliseconds(500))) {
        if (parameters_[cursor_] < param_info[cursor_].upper_limit_) {
            parameters_[cursor_] += param_info[cursor_].increment_;

            if (long_hold_time_[0] > milliseconds(2000)) {
                parameters_[cursor_] += param_info[cursor_].increment_ * 9;
            }
        }
        update_parameter(cursor_);
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
        update_parameter(cursor_);
        app.player().key_held_reset(Key::left, milliseconds(80));

        if (long_hold_time_[0] > milliseconds(2000)) {
            parameters_[cursor_] += param_info[cursor_].increment_ * 3;
        }
    }

    if (app.player().key_down(pfrm, Key::down) and
        cursor_ < parameters_.size() - 1) {
        ++cursor_;

    } else if (app.player().key_down(pfrm, Key::up) and cursor_ > 0) {
        --cursor_;
    }


    return null_scene();
}



SandboxLoaderModule::Factory SandboxLoaderModule::factory_;



} // namespace skyland
