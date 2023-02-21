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
#include "lispReplScene.hpp"
#include "loadLevelScene.hpp"
#include "macro/freebuildConnectFriendScene.hpp"
#include "macro/macroverseScene.hpp"
#include "macro/modifiedSelectorScene.hpp"
#include "macro/nextTurnScene.hpp"
#include "macro/saveConfirmScene.hpp"
#include "macro/selectorScene.hpp"
#include "modules/glossaryViewerModule.hpp"
#include "qrViewerScene.hpp"
#include "readyScene.hpp"
#include "saveSandboxScene.hpp"
#include "selectChallengeScene.hpp"
#include "selectSampleScene.hpp"
#include "skyland/macrocosmFreebuildSector.hpp"
#include "skyland/player/player.hpp"
#include "skyland/player/sandboxSpectatorPlayer.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "spectatorScene.hpp"
#include "surrenderConfirmScene.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



StartMenuScene::StartMenuScene(int fade_direction, int default_cursor)
    : data_(allocate_dynamic<Data>("start-menu-options-buffer")),
      fade_direction_(fade_direction)
{
    data_->cursor_ = default_cursor;
}



void StartMenuScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    pfrm.fill_overlay(0);

    if (app.game_mode() == App::GameMode::macro) {
        start_y_ = 0;
    }
}



void StartMenuScene::add_option(Platform& pfrm,
                                const char* str,
                                DeferredScene on_click,
                                TransitionMode transition_mode)
{
    u8 margin = centered_text_margins(pfrm, utf8::len(str));

    data_->text_.emplace_back(
        pfrm,
        str,
        OverlayCoord{margin, (u8)(start_y_ + data_->text_.size() * 2)});

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



static const char* fb_save_file = "/save/fbld.dat";



class GenerateAgainScene : public macro::MacrocosmScene
{
public:
    void enter(Platform& pfrm, macro::EngineImpl& state, Scene& prev) override
    {
        StringBuffer<30> text(SYSTR(repeat_query)->c_str());

        auto st = calc_screen_tiles(pfrm);

        const int count = st.x - text_->len();
        for (int i = 0; i < count; ++i) {
            pfrm.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
        }

        for (int i = 0; i < st.x; ++i) {
            pfrm.set_tile(Layer::overlay, i, st.y - 2, 425);
        }

        yes_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
        no_text_.emplace(pfrm, OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

        yes_text_->assign(SYSTR(salvage_option_A)->c_str());
        no_text_->assign(SYSTR(salvage_option_B)->c_str());

        for (int i = 23; i < st.x; ++i) {
            pfrm.set_tile(Layer::overlay, i, st.y - 4, 425);
        }

        pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
        pfrm.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

        text_.emplace(pfrm, text.c_str(), OverlayCoord{0, u8(st.y - 1)});

        draw_compass(pfrm, state);
    }


    void exit(Platform& pfrm, macro::EngineImpl&, Scene& prev) override
    {
        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        const auto st = calc_screen_tiles(pfrm);
        for (int x = 0; x < st.x; ++x) {
            pfrm.set_tile(Layer::overlay, x, st.y - 1, 0);
            pfrm.set_tile(Layer::overlay, x, st.y - 2, 0);
            pfrm.set_tile(Layer::overlay, x, st.y - 3, 0);
            pfrm.set_tile(Layer::overlay, x, st.y - 4, 0);
        }
    }


    virtual ScenePtr<Scene>
    update(Platform& pfrm, Player& player, macro::EngineImpl& state)
    {
        auto& sector = state.sector();

        if (player.key_down(pfrm, Key::left)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            sector.render(pfrm);
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(pfrm, Key::right)) {
            pfrm.screen().schedule_fade(0.7f, custom_color(0x102447));
            pfrm.screen().clear();
            pfrm.screen().display();
            sector.rotate();
            sector.rotate();
            sector.rotate();
            sector.render(pfrm);
            pfrm.screen().schedule_fade(0.f, ColorConstant::rich_black);
            draw_compass(pfrm, state);
            pfrm.speaker().play_sound("cursor_tick", 0);
        }

        if (player.key_down(pfrm, Key::action_1)) {
            pfrm.speaker().play_sound("button_wooden", 3);
            auto& current = state.sector();
            current.generate_terrain(160, 1);
            auto sz = current.size();
            current.set_cursor({u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)});
            return null_scene();
        }
        if (player.key_down(pfrm, Key::action_2)) {
            return scene_pool::alloc<macro::SelectorScene>();
        }

        return null_scene();
    }


private:
    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
};



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


    auto add_macro_share_opt = [&] {
        add_option(
            pfrm,
            SYSTR(start_menu_share)->c_str(),
            [&pfrm, &app]() -> ScenePtr<Scene> {
                pfrm.fill_overlay(0);
                Text t(pfrm,
                       SYSTR(macro_share_please_wait)->c_str(),
                       OverlayCoord{1, 3});
                pfrm.screen().clear();
                pfrm.screen().display();

                Text info(pfrm, "", OverlayCoord{1, 1});

                auto msg = [&pfrm, &info](const char* text) {
                    pfrm.screen().clear();
                    info.assign(text);
                    pfrm.screen().display();
                };

                // Just in case the variable isn't bound...
                lisp::set_var("on-dialog-closed", L_NIL);


                auto& current = macrocosm(app).sector();
                auto qr = current.qr_encode(pfrm, app, msg);
                if (qr) {
                    pfrm.screen().pixelate(0);
                    pfrm.fill_overlay(0);
                    auto show_qr = [&pfrm, code = *qr]() -> ScenePtr<Scene> {
                        return scene_pool::alloc<QRViewerScene>(
                            code,
                            [&pfrm]() {
                                pfrm.load_overlay_texture("overlay");
                                pfrm.load_background_texture(
                                    "background_macro");
                                pfrm.screen().schedule_fade(0.f);
                                return scene_pool::alloc<
                                    macro::SelectorScene>();
                            },
                            ColorConstant::rich_black);
                    };
                    if (qr->size() > 76) {
                        pfrm.screen().schedule_fade(0.f);
                        auto dialog =
                            allocate_dynamic<DialogString>("dialog-buffer");
                        *dialog = SYS_CSTR(qr_code_size_warning);
                        auto next = scene_pool::alloc<BoxedDialogScene>(
                            std::move(dialog), false);
                        next->set_next_scene(show_qr);
                        return next;
                    } else {
                        return show_qr();
                    }
                } else {
                    pfrm.screen().schedule_fade(0.f);
                    pfrm.screen().pixelate(0);

                    auto dialog =
                        allocate_dynamic<DialogString>("dialog-buffer");
                    *dialog = SYS_CSTR(qr_code_size_error);
                    auto next = scene_pool::alloc<BoxedDialogScene>(
                        std::move(dialog), false);
                    next->set_next_scene(scene_pool::make_deferred_scene<
                                         macro::SelectorScene>());

                    return next;
                }
            },
            fade_sweep);
    };


    switch (state_) {
    case State::init: {
        pfrm.load_overlay_texture("overlay_challenges");

        if (app.game_mode() == App::GameMode::macro) {

            add_option(pfrm,
                       SYSTR(start_menu_resume)->c_str(),
                       scene_pool::make_deferred_scene<macro::SelectorScene>(),
                       kill_menu);

            if (not macrocosm(app).data_->freebuild_mode_ and
                not macrocosm(app).data_->checkers_mode_) {

                // These options don't apply to freebuild_mode_.

                diff_percent_ = 0.3f;

                add_option(
                    pfrm,
                    SYSTR(start_menu_next_turn)->c_str(),
                    scene_pool::make_deferred_scene<macro::NextTurnScene>(),
                    cut);

                add_option(
                    pfrm,
                    SYSTR(start_menu_macroverse)->c_str(),
                    scene_pool::make_deferred_scene<macro::MacroverseScene>(),
                    fade_sweep_transparent_text);
            }

        } else /* Game mode not_eq macro  */ {

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


        if (not pfrm.network_peer().is_connected() and
            app.game_mode() not_eq App::GameMode::sandbox and
            (app.game_mode() not_eq App::GameMode::macro or
             (app.game_mode() == App::GameMode::macro and
              not macrocosm(app).data_->freebuild_mode_))) {
            // Don't support the hibernate feature for active multiplayer
            // games. On some devices, a serial interrupt for multiplayer will
            // wake the system out of low power mode anyway.

            add_option(pfrm,
                       SYSTR(start_menu_hibernate)->c_str(),
                       scene_pool::make_deferred_scene<HibernateScene>(),
                       fade_sweep);
        }


        switch (app.game_mode()) {
        case App::GameMode::sandbox:
            diff_percent_ = -0.1f;

            add_option(
                pfrm,
                SYSTR(start_menu_repl)->c_str(),
                [&app]() { return scene_pool::alloc<LispReplScene>(); },
                cut);

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
                SYSTR(start_menu_spectate)->c_str(),
                [&pfrm, &app]() -> ScenePtr<Scene> {
                    app.swap_player<SandboxSpectatorPlayer>(app);
                    pfrm.screen().schedule_fade(0.f);
                    return scene_pool::alloc<SpectatorScene>();
                },
                cut);

            add_option(
                pfrm,
                SYSTR(start_menu_sandbox_help)->c_str(),
                [&pfrm] {
                    auto hint = lisp::get_var("sb-help");
                    if (hint->type() == lisp::Value::Type::function) {

                        using namespace lisp;

                        safecall(hint, 0);
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

            if (macrocosm(app).data_->checkers_mode_) {
                add_option(
                    pfrm,
                    SYSTR(start_menu_quit)->c_str(),
                    [&pfrm]() -> ScenePtr<Scene> {
                        return scene_pool::alloc<TitleScreenScene>(3);
                    },
                    fade_sweep);
                break;
            } else if (macrocosm(app).data_->freebuild_mode_) {

                diff_percent_ = 0.18f;

                if (not pfrm.network_peer().is_connected()) {
                    // NOTE: Don't display the connect or load options if we're
                    // already in a multiplayer session.

                    auto& current = macrocosm(app).sector();
                    using macro::terrain::FreebuildSector;
                    if (current.cast_freebuild_sector()) {
                        add_option(
                            pfrm,
                            SYSTR(start_menu_link)->c_str(),
                            [&pfrm, &app]() -> ScenePtr<Scene> {
                                pfrm.screen().pixelate(0);
                                using Next = macro::FreebuildConnectFriendScene;
                                return scene_pool::alloc<Next>();
                            },
                            fade_sweep);
                    }

                    add_macro_share_opt();

                    add_option(
                        pfrm,
                        SYSTR(start_menu_freebuild_samples)->c_str(),
                        [&pfrm, &app]() {
                            return scene_pool::alloc<SelectSampleScene>();
                        },
                        cut);

                    add_option(
                        pfrm,
                        SYSTR(start_menu_freebuild_gen_terrain)->c_str(),
                        [&pfrm, &app]() {
                            auto& current = macrocosm(app).sector();
                            current.generate_terrain(160, 1);
                            auto sz = current.size();
                            current.set_cursor(
                                {u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)});
                            pfrm.screen().schedule_fade(0.f);
                            pfrm.screen().pixelate(0);
                            return scene_pool::alloc<GenerateAgainScene>();
                        },
                        cut);

                    add_option(
                        pfrm,
                        SYSTR(start_menu_repl)->c_str(),
                        [&app]() { return scene_pool::alloc<LispReplScene>(); },
                        cut);

                    add_option(
                        pfrm,
                        SYSTR(start_menu_load)->c_str(),
                        [&pfrm, &app]() -> ScenePtr<Scene> {
                            auto& m = macrocosm(app);

                            using namespace macro::terrain;

                            Vector<char> data;
                            if (flash_filesystem::read_file_data_binary(
                                    pfrm, fb_save_file, data)) {

                                auto& current = m.sector();
                                auto coord = current.coordinate();
                                m.erase_sector(coord);

                                switch ((Sector::Shape)data[0]) {
                                case Sector::Shape::freebuild:
                                case Sector::Shape::freebuild_wide:
                                case Sector::Shape::freebuild_flat:
                                    break;

                                default:
                                    Platform::fatal(
                                        "load fb sector, bad shape");
                                }

                                m.make_sector(coord, (Sector::Shape)data[0]);
                                if (auto s = m.bind_sector(coord)) {
                                    s->unpack(data);
                                }
                            }

                            pfrm.screen().schedule_fade(0.f);
                            pfrm.screen().pixelate(0);
                            return scene_pool::alloc<macro::SelectorScene>();
                        },
                        cut);
                }

                add_option(
                    pfrm,
                    SYSTR(start_menu_save)->c_str(),
                    [&pfrm, &app]() -> ScenePtr<Scene> {
                        auto& current = macrocosm(app).sector();
                        Vector<char> data;
                        current.pack(data);

                        flash_filesystem::store_file_data_binary(
                            pfrm, fb_save_file, data);

                        pfrm.screen().schedule_fade(0.f);
                        pfrm.screen().pixelate(0);
                        return scene_pool::alloc<macro::SelectorScene>();
                    },
                    cut);

                add_option(
                    pfrm,
                    SYSTR(start_menu_quit)->c_str(),
                    [&pfrm]() -> ScenePtr<Scene> {
                        if (pfrm.network_peer().is_connected()) {
                            pfrm.network_peer().disconnect();
                        }
                        pfrm.fill_overlay(0);
                        pfrm.screen().set_shader(passthrough_shader);
                        return scene_pool::alloc<TitleScreenScene>(4);
                    },
                    fade_sweep);
                break;
            }

            add_option(
                pfrm,
                SYSTR(start_menu_save)->c_str(),
                [&pfrm, &app]() -> ScenePtr<Scene> {
                    return scene_pool::alloc<macro::SaveConfirmScene>();
                },
                fade_sweep);

            add_macro_share_opt();

            add_option(
                pfrm,
                SYSTR(start_menu_newgame)->c_str(),
                [&pfrm, &app]() -> ScenePtr<Scene> {
                    macrocosm(app).newgame(pfrm, app);
                    pfrm.load_overlay_texture("overlay_challenges");
                    return scene_pool::alloc<macro::MacroverseScene>();
                },
                cut);


            add_option(
                pfrm,
                SYSTR(start_menu_quit)->c_str(),
                [&pfrm]() -> ScenePtr<Scene> {
                    pfrm.fill_overlay(0);
                    pfrm.screen().set_shader(passthrough_shader);
                    return scene_pool::alloc<TitleScreenScene>(4);
                },
                fade_sweep);

            break;

        case App::GameMode::adventure:
            if (app.opponent_island() == nullptr or
                app.world_graph().nodes_[app.current_world_location()].type_ ==
                    WorldGraph::Node::Type::shop) {
                add_option(pfrm,
                           SYSTR(start_menu_sky_map)->c_str(),
                           scene_pool::make_deferred_scene<ZoneImageScene>(),
                           cut);
            } else {
                if (not app.opponent().is_friendly()) {
                    add_option(pfrm,
                               SYSTR(start_menu_end_run)->c_str(),
                               scene_pool::make_deferred_scene<
                                   SurrenderConfirmScene>(),
                               fade_sweep);
                }

                bool is_final_boss =
                    app.world_graph()
                        .nodes_[app.current_world_location()]
                        .type_ == WorldGraph::Node::Type::corrupted;

                if (app.has_backup() and not is_final_boss) {
                    add_option(
                        pfrm,
                        SYSTR(retry)->c_str(),
                        [&pfrm, &app]() -> ScenePtr<Scene> {
                            pfrm.fill_overlay(0);
                            app.restore_backup(pfrm);
                            pfrm.speaker().clear_sounds();
                            return scene_pool::alloc<LoadLevelScene>();
                        },
                        fade_sweep);
                }
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
                    auto next = scene_pool::alloc<ReadyScene>();
                    next->set_gamespeed(pfrm, app, GameSpeed::normal);
                    return next;
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

                        safecall(hint, 0);
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
            } else if (fade_direction_ == -1) {
                pfrm.screen().pixelate(128, false);
                // skip...
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
