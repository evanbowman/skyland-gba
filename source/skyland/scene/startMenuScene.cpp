////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "startMenuScene.hpp"
#include "boxedDialogScene.hpp"
#include "hibernateScene.hpp"
#include "hideRoomsScene.hpp"
#include "levelExitScene.hpp"
#include "lispReplScene.hpp"
#include "loadLevelScene.hpp"
#include "macro/freebuildConnectFriendScene.hpp"
#include "macro/macroverseScene.hpp"
#include "macro/modifiedSelectorScene.hpp"
#include "macro/nextTurnScene.hpp"
#include "macro/saveConfirmScene.hpp"
#include "macro/selectorScene.hpp"
#include "modules/fileBrowserModule.hpp"
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
#include "worldMapScene.hpp"
#include "zoneImageScene.hpp"



namespace skyland
{



StartMenuScene::StartMenuScene(int fade_direction, int default_cursor)
    : data_(allocate_dynamic<Data>("start-menu-options-buffer")),
      fade_direction_(fade_direction)
{
    data_->cursor_ = default_cursor;
}



void StartMenuScene::enter(Scene& prev)
{
    PLATFORM.fill_overlay(0);

    hide_translucence();

    if (APP.game_mode() == App::GameMode::macro) {
        start_y_ = 0;
    }

    if (not cascade_anim_in_) {
        diff_percent_ *= -1.f;
        add_offset_ = -30;
    }
}



void StartMenuScene::add_option(const char* str,
                                DeferredScene on_click,
                                TransitionMode transition_mode)
{
    data_->on_click_.push_back({on_click, transition_mode});
    data_->option_names_.push_back(str);
    data_->disp_queue_.push_back(str);
}



void StartMenuScene::exit(Scene& next)
{
    PLATFORM.screen().pixelate(0);
    data_->option_names_.clear();
}



static const char* fb_save_file = "/save/fbld.dat";



class GenerateAgainScene : public macro::MacrocosmScene
{
public:
    void enter(macro::EngineImpl& state, Scene& prev) override
    {
        StringBuffer<30> text(SYSTR(repeat_query)->c_str());

        auto st = calc_screen_tiles();

        const int count = st.x - text_->len();
        for (int i = 0; i < count; ++i) {
            PLATFORM.set_tile(Layer::overlay, i + text_->len(), st.y - 1, 426);
        }

        for (int i = 0; i < st.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 2, 425);
        }

        yes_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 3)});
        no_text_.emplace(OverlayCoord{u8(st.x - 7), u8(st.y - 2)});

        yes_text_->assign(SYSTR(salvage_option_A)->c_str());
        no_text_->assign(SYSTR(salvage_option_B)->c_str());

        for (int i = 23; i < st.x; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, st.y - 4, 425);
        }

        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 2, 419);
        PLATFORM.set_tile(Layer::overlay, st.x - 8, st.y - 3, 130);

        text_.emplace(text.c_str(), OverlayCoord{0, u8(st.y - 1)});

        draw_compass(state);
    }


    void exit(macro::EngineImpl&, Scene& prev) override
    {
        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        const auto st = calc_screen_tiles();
        for (int x = 0; x < st.x; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, st.y - 1, 0);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 2, 0);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 3, 0);
            PLATFORM.set_tile(Layer::overlay, x, st.y - 4, 0);
        }
    }


    virtual ScenePtr update(Player& player, macro::EngineImpl& state) override
    {
        auto& sector = state.sector();

        if (player.key_down(Key::left)) {
            PLATFORM.screen().schedule_fade(0.7f, {custom_color(0x102447)});
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            sector.rotate();
            sector.render();
            PLATFORM.screen().schedule_fade(0.f, {ColorConstant::rich_black});
            draw_compass(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        } else if (player.key_down(Key::right)) {
            PLATFORM.screen().schedule_fade(0.7f, {custom_color(0x102447)});
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            sector.rotate();
            sector.rotate();
            sector.rotate();
            sector.render();
            PLATFORM.screen().schedule_fade(0.f, {ColorConstant::rich_black});
            draw_compass(state);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if (player.key_down(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            auto& current = state.sector();
            current.generate_terrain(160, 1);
            auto sz = current.size();
            current.set_cursor({u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)});
            return null_scene();
        }
        if (player.key_down(Key::action_2)) {
            return make_scene<macro::SelectorScene>();
        }

        return null_scene();
    }


private:
    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
};



void restore_overworld_textures()
{
    auto& isle = APP.player_island();
    if (isle.interior_visible()) {
        auto t = APP.environment().player_island_interior_texture();
        PLATFORM.load_tile0_texture(t);
    } else {
        PLATFORM.load_tile0_texture(APP.environment().player_island_texture());
    }
    if (isle.interior_visible()) {
        show_island_interior(&APP.player_island());
    } else {
        show_island_exterior(&APP.player_island());
    }
    PLATFORM.set_scroll(isle.layer(),
                        -isle.get_position().x.as_integer(),
                        -isle.get_position().y.as_integer());

    auto view = PLATFORM.screen().get_view();
    view.set_center(APP.camera()->center());
    PLATFORM.screen().set_view(view);
}



ScenePtr StartMenuScene::update(Time delta)
{
    player().update(delta);

    auto test_key = [&](Key k) {
        return player().test_key(k, milliseconds(500), milliseconds(100));
    };

AGAIN:
    if (not data_->disp_queue_.empty()) {
        data_->disp_timer_ -= delta;
        if (not cascade_anim_in_ or data_->disp_timer_ <= 0) {
            data_->disp_timer_ =
                milliseconds(20 + 2 * data_->disp_queue_.size());

            auto front = *data_->disp_queue_.begin();
            data_->disp_queue_.erase(data_->disp_queue_.begin());

            u8 margin = centered_text_margins(utf8::len(front.c_str()));

            data_->text_.emplace_back(

                front.c_str(),
                OverlayCoord{margin, (u8)(start_y_ + data_->text_.size() * 2)});

            if (cascade_anim_in_) {
                PLATFORM.speaker().play_sound("cursor_tick", 4);
            } else {
                goto AGAIN;
            }
        }
    }

    auto check_button = [&] {
        if (player().key_down(Key::action_1)) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
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

            SYSTR(start_menu_share)->c_str(),
            []() -> ScenePtr {
                PLATFORM.fill_overlay(0);
                Text t(SYSTR(macro_share_please_wait)->c_str(),
                       OverlayCoord{1, 3});
                PLATFORM.screen().clear();
                PLATFORM.screen().display();

                Text info("", OverlayCoord{1, 1});

                auto msg = [&info](const char* text) {
                    PLATFORM.screen().clear();
                    info.assign(text);
                    PLATFORM.screen().display();
                };

                // Just in case the variable isn't bound...
                lisp::set_var("on-dialog-closed", L_NIL);


                auto& current = macrocosm().sector();
                auto qr = current.qr_encode(msg);
                if (qr) {
                    PLATFORM.screen().pixelate(0);
                    PLATFORM.fill_overlay(0);
                    auto show_qr = [&code = *qr]() -> ScenePtr {
                        return make_scene<QRViewerScene>(
                            code,
                            []() {
                                PLATFORM.load_overlay_texture("overlay");
                                PLATFORM.load_background_texture(
                                    "background_macro");
                                PLATFORM.screen().schedule_fade(0.f);
                                return make_scene<macro::SelectorScene>();
                            },
                            ColorConstant::rich_black);
                    };

                    return show_qr();

                } else {
                    PLATFORM.screen().schedule_fade(0.f);
                    PLATFORM.screen().pixelate(0);

                    auto dialog =
                        allocate_dynamic<DialogString>("dialog-buffer");
                    *dialog = SYS_CSTR(qr_code_size_error);
                    auto next = make_scene<BoxedDialogScene>(std::move(dialog));
                    next->set_next_scene(
                        make_deferred_scene<macro::SelectorScene>());

                    return next;
                }
            },
            fade_sweep);
    };


    switch (state_) {
    case State::init: {
        PLATFORM.load_overlay_texture("overlay_challenges");

        if (APP.game_mode() == App::GameMode::macro) {

            add_option(SYSTR(start_menu_resume)->c_str(),
                       make_deferred_scene<macro::SelectorScene>(),
                       kill_menu);

            if (not macrocosm().data_->freebuild_mode_ and
                not macrocosm().data_->checkers_mode_) {

                // These options don't apply to freebuild_mode_.

                diff_percent_ = 0.3f;
                if (not cascade_anim_in_) {
                    diff_percent_ *= -1.f;
                    add_offset_ = -30;
                }

                add_option(

                    SYSTR(start_menu_next_turn)->c_str(),
                    make_deferred_scene<macro::NextTurnScene>(),
                    cut);

                add_option(

                    SYSTR(start_menu_macroverse)->c_str(),
                    make_deferred_scene<macro::MacroverseScene>(),
                    fade_sweep_transparent_text);
            }

        } else /* Game mode not_eq macro  */ {

            add_option(SYSTR(start_menu_resume)->c_str(),
                       make_deferred_scene<ReadyScene>(),
                       kill_menu);

            add_option(

                SYSTR(start_menu_glossary)->c_str(),
                [] {
                    auto next = make_scene<GlossaryViewerModule>();
                    next->disable_backdrop_ = true;
                    next->set_next_scene([]() {
                        restore_overworld_textures();
                        auto next = make_scene<StartMenuScene>(1);
                        return next;
                    });
                    return next;
                },
                cut);

            add_option(

                SYSTR(start_menu_disable_rooms)->c_str(),
                [] {
                    auto next = make_scene<HideRoomsScene>(
                        []() { return make_scene<StartMenuScene>(1); });
                    return next;
                },
                fade_sweep);
        }


        if (not PLATFORM.network_peer().is_connected() and
            APP.game_mode() not_eq App::GameMode::sandbox and
            (APP.game_mode() not_eq App::GameMode::macro or
             (APP.game_mode() == App::GameMode::macro and
              not macrocosm().data_->freebuild_mode_))) {
            // Don't support the hibernate feature for active multiplayer
            // games. On some devices, a serial interrupt for multiplayer will
            // wake the system out of low power mode anyway.

            add_option(SYSTR(start_menu_hibernate)->c_str(),
                       make_deferred_scene<HibernateScene>(),
                       fade_sweep);
        }


        switch (APP.game_mode()) {
        case App::GameMode::sandbox:
            diff_percent_ = -0.1f;
            add_offset_ = 0;

            add_option(
                SYSTR(start_menu_repl)->c_str(),
                []() { return make_scene<LispReplScene>(); },
                cut);

            add_option(SYSTR(start_menu_save_sandbox)->c_str(),
                       make_deferred_scene<SaveSandboxScene>(),
                       fade_sweep);

            add_option(SYSTR(start_menu_load_sandbox)->c_str(),
                       make_deferred_scene<LoadSandboxScene>(),
                       fade_sweep);

            add_option(
                SYSTR(start_menu_spectate)->c_str(),
                []() -> ScenePtr {
                    APP.swap_player<SandboxSpectatorPlayer>();
                    PLATFORM.screen().schedule_fade(0.f);
                    show_phase();
                    return make_scene<SpectatorScene>();
                },
                cut);

            add_option(
                SYSTR(start_menu_sandbox_help)->c_str(),
                [] {
                    auto hint = lisp::get_var("sb-help");
                    if (hint->type() == lisp::Value::Type::function) {

                        using namespace lisp;

                        safecall(hint, 0);
                        pop_op(); // result

                        PLATFORM.screen().schedule_fade(0.f);

                        return make_scene<ReadyScene>();
                    }
                    Platform::fatal("invalid datatype for challenge-hint"
                                    " (expected function)");
                },
                cut);

            add_option(

                SYSTR(start_menu_quit)->c_str(),
                []() -> ScenePtr {
                    lisp::set_var("sb-help", L_NIL);
                    PLATFORM.fill_overlay(0);
                    PLATFORM.screen().set_shader(passthrough_shader);
                    return make_scene<LevelExitScene<TitleScreenScene>>(3);
                },
                fade_sweep);
            break;

        case App::GameMode::macro:

            if (macrocosm().data_->checkers_mode_) {
                add_option(

                    SYSTR(start_menu_quit)->c_str(),
                    []() -> ScenePtr {
                        return make_scene<TitleScreenScene>(3);
                    },
                    fade_sweep);
                break;
            } else if (macrocosm().data_->freebuild_mode_) {

                diff_percent_ = 0.18f;
                if (not cascade_anim_in_) {
                    diff_percent_ *= -1.f;
                    add_offset_ = -30;
                }


                if (not PLATFORM.network_peer().is_connected()) {

                    add_option(
                        SYSTR(sandbox_music)->c_str(),
                        [] {
                            UserContext ctx;
                            ctx.allow_backtrack_ = false;
                            ctx.browser_exit_scene_ =
                                make_deferred_scene<macro::SelectorScene>();
                            const char* path = "/scripts/data/music/";
                            auto next = make_scene<FileBrowserModule>(
                                std::move(ctx), path, true);
                            next->on_select_ = [](const char* path) {
                                PLATFORM.speaker().stream_music(path, 0);
                            };
                            return next;
                        },
                        cut);

                    add_macro_share_opt();

                    add_option(

                        SYSTR(start_menu_freebuild_samples)->c_str(),
                        []() { return make_scene<SelectSampleScene>(); },
                        cut);

                    add_option(

                        SYSTR(start_menu_freebuild_gen_terrain)->c_str(),
                        []() {
                            auto& current = macrocosm().sector();
                            current.generate_terrain(160, 1);
                            auto sz = current.size();
                            current.set_cursor(
                                {u8(sz.x / 2), u8(sz.y / 2), u8(sz.z / 2)});
                            PLATFORM.screen().schedule_fade(0.f);
                            PLATFORM.screen().pixelate(0);
                            return make_scene<GenerateAgainScene>();
                        },
                        cut);

                    add_option(

                        SYSTR(start_menu_repl)->c_str(),
                        []() { return make_scene<LispReplScene>(); },
                        cut);

                    add_option(

                        SYSTR(start_menu_load)->c_str(),
                        []() -> ScenePtr {
                            auto& m = macrocosm();

                            using namespace macro::terrain;

                            Vector<char> data;
                            if (flash_filesystem::read_file_data_binary(
                                    fb_save_file, data)) {

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

                            PLATFORM.screen().schedule_fade(0.f);
                            PLATFORM.screen().pixelate(0);
                            return make_scene<macro::SelectorScene>();
                        },
                        cut);
                }

                add_option(

                    SYSTR(start_menu_save)->c_str(),
                    []() -> ScenePtr {
                        auto& current = macrocosm().sector();
                        Vector<char> data;
                        current.pack(data);

                        flash_filesystem::store_file_data_binary(fb_save_file,
                                                                 data);

                        PLATFORM.screen().schedule_fade(0.f);
                        PLATFORM.screen().pixelate(0);
                        return make_scene<macro::SelectorScene>();
                    },
                    cut);

                add_option(

                    SYSTR(start_menu_quit)->c_str(),
                    []() -> ScenePtr {
                        if (PLATFORM.network_peer().is_connected()) {
                            PLATFORM.network_peer().disconnect();
                        }
                        PLATFORM.fill_overlay(0);
                        PLATFORM.screen().set_shader(passthrough_shader);
                        if (macrocosm().data_->freebuild_mode_) {
                            return make_scene<TitleScreenScene>( // 3
                                4);
                        } else {
                            return make_scene<TitleScreenScene>(4);
                        }
                    },
                    fade_sweep);
                break;
            }

            add_option(

                SYSTR(start_menu_save)->c_str(),
                []() -> ScenePtr {
                    return make_scene<macro::SaveConfirmScene>();
                },
                fade_sweep);

            add_macro_share_opt();

            add_option(

                SYSTR(start_menu_newgame)->c_str(),
                []() -> ScenePtr {
                    Text("generating world...", OverlayCoord{1, 1});
                    PLATFORM.screen().schedule_fade(0);
                    PLATFORM.screen().schedule_fade(1);
                    PLATFORM.screen().clear();
                    PLATFORM.screen().display();
                    macrocosm().newgame();
                    PLATFORM.load_overlay_texture("overlay_challenges");
                    return make_scene<macro::MacroverseScene>();
                },
                cut);

            add_option(

                SYSTR(start_menu_quit)->c_str(),
                []() -> ScenePtr {
                    PLATFORM.fill_overlay(0);
                    PLATFORM.screen().set_shader(passthrough_shader);
                    return make_scene<TitleScreenScene>(4);
                },
                fade_sweep);

            break;

        case App::GameMode::adventure:
            if (APP.is_developer_mode()) {
                add_option(
                    SYSTR(start_menu_repl)->c_str(),
                    []() { return make_scene<LispReplScene>(); },
                    cut);
            }
            if (APP.opponent_island() == nullptr or
                APP.world_graph().nodes_[APP.current_world_location()].type_ ==
                    WorldGraph::Node::Type::shop) {
                add_option(
                    SYSTR(start_menu_sky_map)->c_str(),
                    []() -> ScenePtr {
                        if (APP.current_world_location() == 0) {
                            return make_scene<LevelExitScene<ZoneImageScene>>();
                        } else {
                            return make_scene<LevelExitScene<ZoneImageScene>>();
                        }
                    },
                    cut);
            } else {
                if (not APP.opponent().is_friendly()) {
                    add_option(SYSTR(start_menu_end_run)->c_str(),
                               make_deferred_scene<SurrenderConfirmScene>(),
                               fade_sweep);
                }

                bool is_final_boss =
                    APP.world_graph()
                        .nodes_[APP.current_world_location()]
                        .type_ == WorldGraph::Node::Type::corrupted;

                if (APP.has_backup() and not is_final_boss) {
                    add_option(

                        SYSTR(retry)->c_str(),
                        []() -> ScenePtr {
                            PLATFORM.fill_overlay(0);
                            APP.restore_backup();
                            PLATFORM.speaker().clear_sounds();
                            return make_scene<LoadLevelScene>();
                        },
                        fade_sweep);
                }
            }
            break;

        case App::GameMode::skyland_forever:
            add_option(

                SYSTR(start_menu_end_run)->c_str(),
                [] {
                    APP.exit_condition() = App::ExitCondition::defeat;
                    PLATFORM.speaker().stop_music();
                    PLATFORM.screen().schedule_fade(0.f);
                    PLATFORM.screen().pixelate(0);
                    auto next = make_scene<ReadyScene>();
                    next->set_gamespeed(GameSpeed::normal);
                    return next;
                },
                cut);
            break;

        case App::GameMode::challenge:
            add_option(

                SYSTR(start_menu_hint)->c_str(),
                [] {
                    auto hint = lisp::get_var("challenge-hint");
                    if (hint->type() == lisp::Value::Type::function) {

                        using namespace lisp;

                        safecall(hint, 0);
                        pop_op(); // result

                        PLATFORM.screen().schedule_fade(0.f);
                        PLATFORM.screen().pixelate(0);

                        return make_scene<ReadyScene>();
                    }
                    Platform::fatal("invalid datatype for challenge-hint"
                                    " (expected function)");
                },
                cut);

            add_option(

                SYSTR(start_menu_quit)->c_str(),
                []() -> ScenePtr {
                    PLATFORM.fill_overlay(0);
                    PLATFORM.screen().set_shader(passthrough_shader);
                    return make_scene<LevelExitScene<SelectChallengeScene>>();
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
        const auto y_center = PLATFORM.screen().size().y / 2;
        const Float y_line = line.coord().y * 8;
        const auto y_diff = (y_line - y_center) * diff_percent_;

        y_offset_ = interpolate(Float(y_diff), y_offset_, delta * 0.00001f);

        PLATFORM.set_overlay_origin(0, y_offset_ + add_offset_);

        timer_ += delta;

        auto step = smoothstep(0, fade_duration, timer_);

        if (timer_ < fade_duration) {
            if (fade_direction_ == 0) {
                PLATFORM.screen().schedule_fade(0.75f * step);
                PLATFORM.screen().pixelate(step * 128, false);
            } else if (fade_direction_ == -1) {
                PLATFORM.screen().pixelate(128, false);
                // skip...
            } else {
                PLATFORM.screen().pixelate(128, false);
                PLATFORM.screen().schedule_fade(1.f - 0.25f * step);
            }

        } else {
            PLATFORM.screen().schedule_fade(0.75f);
            state_ = State::idle;
            timer_ = 0;
        }

        if (test_key(Key::down)) {
            if (data_->cursor_ < data_->text_.size() - 1) {
                ++data_->cursor_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
        if (test_key(Key::up)) {
            if (data_->cursor_ > 0) {
                --data_->cursor_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
        check_button();
        break;
    }

    case State::idle:
        if (player().key_down(Key::action_2) or player().key_down(Key::start)) {
            state_ = State::clear;
        }
        check_button();
        if (test_key(Key::down)) {
            if (data_->cursor_ < data_->text_.size() - 1) {
                ++data_->cursor_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
        if (test_key(Key::up)) {
            if (data_->cursor_ > 0) {
                --data_->cursor_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        }
        break;

    case State::clear: {
        data_->text_.clear();
        state_ = State::exit;
        break;
    }

    case State::cut:
        PLATFORM.set_overlay_origin(0, 0);
        PLATFORM.load_overlay_texture("overlay");
        PLATFORM.screen().schedule_fade(1.f);
        return data_->on_click_[data_->cursor_].next_scene_();

    case State::exit:
        PLATFORM.set_overlay_origin(0, 0);
        PLATFORM.load_overlay_texture("overlay");
        PLATFORM.screen().schedule_fade(0.f);
        PLATFORM.screen().pixelate(0);
        if (APP.game_mode() == App::GameMode::macro) {
            return make_scene<macro::SelectorScene>();
        } else {
            show_phase();
            return make_scene<ReadyScene>();
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
            PLATFORM.screen().schedule_fade(0.75f + 0.25f * step);
        } else {
            PLATFORM.screen().schedule_fade(1.f);
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
            PLATFORM.set_overlay_origin(0, ideal_y);
            timer_ = 0;
            state_ = State::after_sweep;
        } else {
            PLATFORM.set_overlay_origin(0, y);
        }

        break;
    }

    case State::after_sweep: {
        timer_ += delta;
        if (timer_ > milliseconds(100)) {
            PLATFORM_EXTENSION(force_vsync);
            data_->text_.clear();
            PLATFORM.screen().display();

            PLATFORM.set_overlay_origin(0, 0);
            if (not preserve_transparency_) {
                PLATFORM.load_overlay_texture("overlay");
            }

            PLATFORM.screen().schedule_fade(0.f);
            PLATFORM.screen().schedule_fade(1.f);

            auto name = data_->option_names_[data_->cursor_];
            Text text(

                {(u8)centered_text_margins(utf8::len(name.c_str())), 1});
            text.assign(name.c_str());
            text.__detach();

            return data_->on_click_[data_->cursor_].next_scene_();
        }
        break;
    }
    }

    return null_scene();
}



void StartMenuScene::display()
{
    Sprite cursor;
    cursor.set_size(Sprite::Size::w16_h32);
    cursor.set_texture_index(59);

    cursor.set_mix({ColorConstant::silver_white, 1});

    // NOTE: for the gba platform, setting a custom mix color is sufficient to
    // prevent screen fades from applying to the cursor sprite. For other
    // non-paletted platforms, we do need to draw the cursor over the overlay to
    // exclude it from screen fades...
    cursor.set_priority(0);

    auto view = PLATFORM.screen().get_view().get_center();

    Vec2<Fixnum> origin;

    // auto ambient_movement = 2 * float(sine(4 * 3.14f * 0.004f * timer_ + 180)) /
    //                         std::numeric_limits<s16>::max();

    origin.x = ((int)(data_->text_[data_->cursor_].coord().x - 2) * 8) + view.x;
    origin.y = (data_->text_[data_->cursor_].coord().y * 8 - y_offset_) +
               view.y - add_offset_;

    cursor.set_position(origin);

    if (state_ == State::idle or state_ == State::enter) {
        if (state_ == State::enter) {
            cursor.set_alpha(Sprite::Alpha::translucent);
        }
        PLATFORM.screen().draw(cursor);
    }
}



} // namespace skyland
