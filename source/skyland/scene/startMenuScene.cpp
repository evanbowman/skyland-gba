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


#include "startMenuScene.hpp"
#include "boxedDialogScene.hpp"
#include "hibernateScene.hpp"
#include "hideRoomsScene.hpp"
#include "macro/macroverseScene.hpp"
#include "macro/modifiedSelectorScene.hpp"
#include "macro/nextTurnScene.hpp"
#include "macro/selectorScene.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "readyScene.hpp"
#include "saveSandboxScene.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/player/player.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



StartMenuScene::StartMenuScene(int fade_direction)
    : data_(allocate_dynamic<Data>("start-menu-options-buffer")),
      fade_direction_(fade_direction)
{
}



void StartMenuScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);
}



void StartMenuScene::add_option(Platform& pfrm,
                                const char* str,
                                DeferredScene on_click,
                                TransitionMode transition_mode)
{

    int start_y = 3;

    u8 margin = centered_text_margins(pfrm, utf8::len(str));

    data_->text_.emplace_back(
        pfrm,
        str,
        OverlayCoord{margin, (u8)(start_y + data_->text_.size() * 2)});

    data_->on_click_.push_back({on_click, transition_mode});
    data_->option_names_.push_back(str);
}



void StartMenuScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.screen().pixelate(0);
    data_->option_names_.clear();
}



static void scuttle(Platform& pfrm, App& app)
{
    app.on_timeout(pfrm, milliseconds(350), [](Platform& pfrm, App& app) {
        for (auto& room : app.player_island().rooms()) {
            if ((*room->metaclass())->category() == Room::Category::power) {
                room->apply_damage(pfrm, app, Room::health_upper_limit());

                app.on_timeout(
                    pfrm, milliseconds(350), [](Platform& pfrm, App& app) {
                        scuttle(pfrm, app);
                    });
                return;
            }
        }
    });
}



ScenePtr<Scene>
StartMenuScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    player(app).update(pfrm, app, delta);

    auto test_key = [&](Key k) {
        return player(app).test_key(
            pfrm, k, milliseconds(500), milliseconds(100));
    };

    auto check_button = [&] {
        if (player(app).key_down(pfrm, Key::action_1)) {
            pfrm.speaker().play_sound("button_wooden", 3);
            const auto mode = data_->on_click_[data_->cursor_].mode_;
            if (mode == kill_menu) {
                state_ = State::clear;
            } else if (mode == fade_sweep) {
                state_ = State::partial_clear;
            } else if (mode == cut) {
                data_->text_.clear();
                state_ = State::cut;
            } else if (mode == fade_sweep_transparent_text) {
                state_ = State::partial_clear;
                preserve_transparency_ = true;
            }
        }
    };


    switch (state_) {
    case State::init: {
        pfrm.load_overlay_texture("overlay_challenges");

        if (app.game_mode() == App::GameMode::macro) {
            diff_percent_ = 0.3f;
            add_option(pfrm,
                       SYSTR(start_menu_resume)->c_str(),
                       scene_pool::make_deferred_scene<macro::SelectorScene>(),
                       kill_menu);

            add_option(pfrm,
                       SYSTR(start_menu_next_turn)->c_str(),
                       scene_pool::make_deferred_scene<macro::NextTurnScene>(),
                       cut);

            // add_option(
            //     pfrm,
            //     SYSTR(start_menu_adjust_view)->c_str(),
            //     scene_pool::make_deferred_scene<macro::ModifiedSelectorScene>(),
            //     cut);

            add_option(
                pfrm,
                SYSTR(start_menu_macroverse)->c_str(),
                scene_pool::make_deferred_scene<macro::MacroverseScene>(),
                fade_sweep_transparent_text);

        } else {
            add_option(pfrm,
                       SYSTR(start_menu_resume)->c_str(),
                       scene_pool::make_deferred_scene<ReadyScene>(),
                       kill_menu);

            add_option(
                pfrm,
                SYSTR(start_menu_glossary)->c_str(),
                [&pfrm] {
                    auto next = scene_pool::alloc<GlossaryViewerModule>();
                    next->set_next_scene([&pfrm]() {
                        return scene_pool::alloc<StartMenuScene>(1);
                    });
                    return next;
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_disable_rooms)->c_str(),
                [&pfrm] {
                    auto next = scene_pool::alloc<HideRoomsScene>([&pfrm]() {
                        return scene_pool::alloc<StartMenuScene>(1);
                    });
                    return next;
                },
                fade_sweep);
        }


        add_option(pfrm,
                   SYSTR(start_menu_hibernate)->c_str(),
                   scene_pool::make_deferred_scene<HibernateScene>(),
                   fade_sweep);



        switch (app.game_mode()) {
        case App::GameMode::sandbox:
            diff_percent_ = -0.1f;

            add_option(pfrm,
                       SYSTR(start_menu_save_sandbox)->c_str(),
                       scene_pool::make_deferred_scene<SaveSandboxScene>(),
                       fade_sweep);

            add_option(pfrm,
                       SYSTR(start_menu_load_sandbox)->c_str(),
                       scene_pool::make_deferred_scene<LoadSandboxScene>(),
                       fade_sweep);

            add_option(
                pfrm,
                SYSTR(start_menu_sandbox_help)->c_str(),
                [&pfrm] {
                    auto hint = lisp::get_var("sb-help");
                    if (hint->type() == lisp::Value::Type::function) {

                        using namespace lisp;

                        funcall(hint, 0);
                        if (get_op(0)->type() == Value::Type::error) {
                            Platform::fatal("unexpected error (sb-help)!");
                        }
                        pop_op(); // result

                        pfrm.screen().schedule_fade(0.f);

                        return scene_pool::alloc<ReadyScene>();
                    }
                    Platform::fatal("invalid datatype for challenge-hint"
                                    " (expected function)");
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_quit)->c_str(),
                [&pfrm]() -> ScenePtr<Scene> {
                    lisp::set_var("sb-help", L_NIL);
                    pfrm.fill_overlay(0);
                    pfrm.screen().set_shader(passthrough_shader);
                    return scene_pool::alloc<TitleScreenScene>(3);
                },
                fade_sweep);
            break;

        case App::GameMode::macro:

            add_option(
                pfrm,
                SYSTR(start_menu_load)->c_str(),
                [&pfrm, &app]() -> ScenePtr<Scene> {
                    app.macrocosm()->load(pfrm);
                    pfrm.screen().schedule_fade(0.f);
                    pfrm.screen().pixelate(0);
                    return scene_pool::alloc<macro::SelectorScene>();
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_save)->c_str(),
                [&pfrm, &app]() -> ScenePtr<Scene> {
                    app.macrocosm()->save(pfrm);
                    pfrm.screen().schedule_fade(0.f);
                    pfrm.screen().pixelate(0);
                    return scene_pool::alloc<macro::SelectorScene>();
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_quit)->c_str(),
                [&pfrm]() -> ScenePtr<Scene> {
                    pfrm.fill_overlay(0);
                    pfrm.screen().set_shader(passthrough_shader);
                    return scene_pool::alloc<TitleScreenScene>(3);
                },
                fade_sweep);
            break;

        case App::GameMode::adventure:
            if (app.opponent_island() == nullptr) {
                add_option(pfrm,
                           SYSTR(start_menu_sky_map)->c_str(),
                           scene_pool::make_deferred_scene<ZoneImageScene>(),
                           cut);
            }
            break;

        case App::GameMode::skyland_forever:
            add_option(
                pfrm,
                SYSTR(start_menu_scuttle)->c_str(),
                [&pfrm, &app] {
                    scuttle(pfrm, app);
                    pfrm.screen().schedule_fade(0.f);
                    pfrm.screen().pixelate(0);
                    app.game_speed() = GameSpeed::normal;
                    return scene_pool::alloc<ReadyScene>();
                },
                cut);
            break;

        case App::GameMode::challenge:
            add_option(
                pfrm,
                SYSTR(start_menu_hint)->c_str(),
                [&pfrm] {
                    auto hint = lisp::get_var("challenge-hint");
                    if (hint->type() == lisp::Value::Type::function) {

                        using namespace lisp;

                        funcall(hint, 0);
                        if (get_op(0)->type() == Value::Type::error) {
                            Platform::fatal("unexpected error (ch hint)!");
                        }
                        pop_op(); // result

                        pfrm.screen().schedule_fade(0.f);
                        pfrm.screen().pixelate(0);

                        return scene_pool::alloc<ReadyScene>();
                    }
                    Platform::fatal("invalid datatype for challenge-hint"
                                    " (expected function)");
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_quit)->c_str(),
                [&pfrm]() -> ScenePtr<Scene> {
                    pfrm.fill_overlay(0);
                    pfrm.screen().set_shader(passthrough_shader);
                    return scene_pool::alloc<SelectChallengeScene>();
                },
                fade_sweep);

            break;

        case App::GameMode::co_op:
        case App::GameMode::multiplayer:
            Platform::fatal("logic error: multiplayer code should "
                            "not open a start menu!");
            break;

        default:
            break;
        }

        state_ = State::enter;
        break;
    }

    case State::enter: {
        static const auto fade_duration = milliseconds(300);

        const auto& line = data_->text_[0];
        const auto y_center = pfrm.screen().size().y / 2;
        const Float y_line = line.coord().y * 8;
        const auto y_diff = (y_line - y_center) * diff_percent_;

        y_offset_ = interpolate(Float(y_diff), y_offset_, delta * 0.00001f);

        pfrm.set_overlay_origin(0, y_offset_);

        timer_ += delta;

        auto step = smoothstep(0, fade_duration, timer_);

        if (timer_ < fade_duration) {
            if (fade_direction_ == 0) {
                pfrm.screen().schedule_fade(0.75f * step);
                pfrm.screen().pixelate(step * 128, false);
            } else {
                pfrm.screen().pixelate(128, false);
                pfrm.screen().schedule_fade(1.f - 0.25f * step);
            }

        } else {
            pfrm.screen().schedule_fade(0.75f);
            state_ = State::idle;
            timer_ = 0;
        }

        if (test_key(Key::down)) {
            if (data_->cursor_ < data_->text_.size() - 1) {
                ++data_->cursor_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        }
        if (test_key(Key::up)) {
            if (data_->cursor_ > 0) {
                --data_->cursor_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        }
        check_button();
        break;
    }

    case State::idle:
        if (player(app).key_down(pfrm, Key::action_2) or
            player(app).key_down(pfrm, Key::start)) {
            state_ = State::clear;
        }
        check_button();
        if (test_key(Key::down)) {
            if (data_->cursor_ < data_->text_.size() - 1) {
                ++data_->cursor_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        }
        if (test_key(Key::up)) {
            if (data_->cursor_ > 0) {
                --data_->cursor_;
                pfrm.speaker().play_sound("click_wooden", 2);
            }
        }
        break;

    case State::clear: {
        data_->text_.clear();
        state_ = State::exit;
        break;
    }

    case State::cut:
        pfrm.set_overlay_origin(0, 0);
        pfrm.load_overlay_texture("overlay");
        pfrm.screen().schedule_fade(1.f);
        return data_->on_click_[data_->cursor_].next_scene_();

    case State::exit:
        pfrm.set_overlay_origin(0, 0);
        pfrm.load_overlay_texture("overlay");
        pfrm.screen().schedule_fade(0.f);
        pfrm.screen().pixelate(0);
        if (app.game_mode() == App::GameMode::macro) {
            return scene_pool::alloc<macro::SelectorScene>();
        } else {
            return scene_pool::alloc<ReadyScene>();
        }


    case State::partial_clear: {
        for (u32 i = 0; i < data_->text_.size(); ++i) {
            if (i not_eq data_->cursor_) {
                data_->text_[i].erase();
            }
        }
        state_ = State::fade_out;
        timer_ = 0;
        break;
    }

    case State::fade_out: {
        static const auto fade_duration = milliseconds(300);

        timer_ += delta;

        auto step = smoothstep(0, fade_duration, timer_);

        if (timer_ < fade_duration) {
            pfrm.screen().schedule_fade(0.75f + 0.25f * step);
        } else {
            pfrm.screen().schedule_fade(1.f);
            state_ = State::sweep_up;
            timer_ = 0;
        }
        break;
    }

    case State::sweep_up: {
        timer_ += delta;
        static const auto scroll_duration = milliseconds(300);

        auto step = smoothstep(0, scroll_duration, timer_);

        const auto& line = data_->text_[data_->cursor_];

        const auto ideal_y = (line.coord().y - 1) * 8.f;
        auto y = interpolate(ideal_y, y_offset_, step);

        if (timer_ > scroll_duration) {
            pfrm.set_overlay_origin(0, ideal_y);
            timer_ = 0;
            state_ = State::after_sweep;
        } else {
            pfrm.set_overlay_origin(0, y);
        }

        break;
    }

    case State::after_sweep: {
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            pfrm.system_call("vsync", nullptr);
            data_->text_.clear();
            pfrm.screen().display();

            pfrm.set_overlay_origin(0, 0);
            if (not preserve_transparency_) {
                pfrm.load_overlay_texture("overlay");
            }

            pfrm.screen().schedule_fade(0.f);
            pfrm.screen().schedule_fade(1.f);

            auto name = data_->option_names_[data_->cursor_];
            Text text(
                pfrm,
                {(u8)centered_text_margins(pfrm, utf8::len(name.c_str())), 1});
            text.assign(name.c_str());
            text.__detach();

            return data_->on_click_[data_->cursor_].next_scene_();
        }
        break;
    }
    }

    return null_scene();
}



void StartMenuScene::display(Platform& pfrm, App& app)
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    cursor.set_mix({ColorConstant::silver_white, 1});

    auto view = pfrm.screen().get_view().get_center();

    Vec2<Fixnum> origin;

    // auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
    //                         std::numeric_limits<s16>::max();

    origin.x = ((int)(data_->text_[data_->cursor_].coord().x - 2) * 8) + view.x;
    origin.y =
        (data_->text_[data_->cursor_].coord().y * 8 - y_offset_) + view.y;

    cursor.set_position(origin);

    if (state_ == State::idle or state_ == State::enter) {
        if (state_ == State::enter) {
            cursor.set_alpha(Sprite::Alpha::translucent);
        }
        pfrm.screen().draw(cursor);
    }
}



} // namespace skyland
