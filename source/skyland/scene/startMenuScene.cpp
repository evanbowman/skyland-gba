#include "startMenuScene.hpp"
#include "boxedDialogScene.hpp"
#include "hibernateScene.hpp"
#include "hideRoomsScene.hpp"
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
    switch (state_) {
    case State::init: {
        pfrm.load_overlay_texture("overlay_challenges");
        add_option(pfrm,
                   SYSTR(start_menu_resume)->c_str(),
                   scene_pool::make_deferred_scene<ReadyScene>(),
                   kill_menu);

        add_option(
            pfrm,
            SYSTR(start_menu_glossary)->c_str(),
            [&pfrm] {
                auto next = scene_pool::alloc<GlossaryViewerModule>();
                next->set_next_scene(
                    [&pfrm]() { return scene_pool::alloc<StartMenuScene>(1); });
                return next;
            },
            cut);

        add_option(pfrm,
                   SYSTR(start_menu_hibernate)->c_str(),
                   scene_pool::make_deferred_scene<HibernateScene>(),
                   fade_sweep);

        add_option(
            pfrm,
            SYSTR(start_menu_disable_rooms)->c_str(),
            [&pfrm] {
                auto next = scene_pool::alloc<HideRoomsScene>(
                    [&pfrm]() { return scene_pool::alloc<StartMenuScene>(1); });
                return next;
            },
            fade_sweep);

        switch (app.game_mode()) {
        case App::GameMode::sandbox:
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
                SYSTR(start_menu_quit)->c_str(),
                [&pfrm]() -> ScenePtr<Scene> {
                    pfrm.fill_overlay(0);
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
        const auto y_diff = (y_line - y_center) * 0.3f;

        y_offset_ = interpolate(Float(y_diff), y_offset_, delta * 0.00001f);

        pfrm.set_overlay_origin(0, y_offset_);

        timer_ += delta;

        auto step = smoothstep(0, fade_duration, timer_);

        if (timer_ < fade_duration) {
            if (fade_direction_ == 0) {
                pfrm.screen().schedule_fade(0.7f * step);
            } else {
                pfrm.screen().schedule_fade(1.f - 0.3f * step);
            }

        } else {
            pfrm.screen().schedule_fade(0.7f);
            state_ = State::idle;
            timer_ = 0;
        }

        break;
    }

    case State::idle:
        if (player(app).key_down(pfrm, Key::action_2) or
            player(app).key_down(pfrm, Key::start)) {
            state_ = State::clear;
        }
        if (player(app).key_down(pfrm, Key::action_1)) {
            const auto mode = data_->on_click_[data_->cursor_].mode_;
            if (mode == kill_menu) {
                state_ = State::clear;
            } else if (mode == fade_sweep) {
                state_ = State::partial_clear;
            } else if (mode == cut) {
                data_->text_.clear();
                state_ = State::cut;
            }
        }
        if (player(app).key_down(pfrm, Key::down)) {
            if (data_->cursor_ < data_->text_.size() - 1) {
                ++data_->cursor_;
            }
        }
        if (player(app).key_down(pfrm, Key::up)) {
            if (data_->cursor_ > 0) {
                --data_->cursor_;
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
        return scene_pool::alloc<ReadyScene>();

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
            pfrm.screen().schedule_fade(0.7f + 0.3f * step);
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
            pfrm.load_overlay_texture("overlay");

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

    Vec2<Float> origin;

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


    WorldScene::display(pfrm, app);
}



} // namespace skyland
