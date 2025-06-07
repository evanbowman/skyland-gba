////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "playerIslandDestroyedScene.hpp"
#include "achievementNotificationScene.hpp"
#include "adventureLogScene.hpp"
#include "boxedDialogScene.hpp"
#include "coOpSyncScene.hpp"
#include "crewStatsScene.hpp"
#include "highscoresScene.hpp"
#include "levelCompleteOptionsScene.hpp"
#include "levelExitScene.hpp"
#include "linkScene.hpp"
#include "platform/color.hpp"
#include "readyScene.hpp"
#include "sandboxResetScene.hpp"
#include "scriptHookScene.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/entity/explosion/exploSpawner.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/network.hpp"
#include "skyland/player/opponent/friendlyAI.hpp"
#include "skyland/player/playerP1.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/rooms/transporter.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/serial.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



StringBuffer<32> format_time(u32 seconds, bool include_hours = true);



namespace skyland
{



static const Float partial_fade_amt = 0.76f;



void PlayerIslandDestroyedScene::show_stats()
{
    const auto screen_tiles = calc_screen_tiles();

    int metrics_y_offset_ = 2;

    const auto dot = ".";

    auto print_metric_impl = [&](const char* str,
                                 const StringBuffer<32>& text,
                                 const char* suffix = "",
                                 bool highlight = false) {
        if (lines_.full()) {
            return;
        }

        lines_.emplace_back(
            RoomCoord{3, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

        const auto colors =
            highlight
                ? Text::OptColors{FontColors{ColorConstant::rich_black,
                                             ColorConstant::aerospace_orange}}
                : Text::OptColors{};


        lines_.back().append(str, colors);

        const auto iters = screen_tiles.x - (utf8::len(str) + 6 +
                                             text.length() + utf8::len(suffix));


        for (u32 i = 0; i < iters; ++i) {
            lines_.back().append(dot, colors);
        }

        lines_.back().append(text.c_str(), colors);
        lines_.back().append(suffix, colors);
    };


    auto print_metric = [&](const char* str,
                            int num,
                            const char* suffix = "",
                            bool highlight = false) {
        print_metric_impl(str, stringify(num), suffix, highlight);
    };

    switch (lines_.size()) {
    case 0: {
        int secs = APP.stat_timer().whole_seconds();
        if (APP.game_mode() == App::GameMode::skyland_forever) {
            secs = APP.persistent_data().total_seconds_.get();
        }
        print_metric_impl(SYSTR(level_complete_time)->c_str(),
                          format_time(secs, true));
        break;
    }

    case 1:
        if (APP.game_mode() == App::GameMode::skyland_forever or
            APP.game_mode() == App::GameMode::co_op) {
        } else {
            print_metric(SYSTR(level_complete_pauses)->c_str(),
                         APP.pause_count());
        }
        break;

    case 2:
        print_metric(SYSTR(level_complete_coins)->c_str(),
                     APP.level_coins_spent());
        break;

    case 3: {
        StringBuffer<24> fmt;
        fmt += stringify(rooms_built_);
        fmt += "/";
        fmt += stringify(rooms_lost_);
        print_metric_impl(SYSTR(level_complete_rooms)->c_str(), fmt.c_str());
        break;
    }

    case 4: {
        if (APP.game_mode() not_eq App::GameMode::skyland_forever) {
            StringBuffer<24> fmt;
            auto score_diff = APP.score().get() - APP.level_begin_score();
            fmt += "+";
            fmt += stringify(score_diff);
            print_metric_impl(SYSTR(highscores_score)->c_str(), fmt.c_str());
        }
        break;
    }
    }
}



void PlayerIslandDestroyedScene::display()
{
    // No overlay circle effect in multiplayer, because the vblank handler could
    // spend too long calculating the circle edges and cause a missed serial
    // interrupt.
    if (not PLATFORM.network_peer().is_connected()) {
        if (circ_effect_radius_ > 70 and last_radius_ < 70) {
            PLATFORM.fill_overlay(496);
        }

        auto pos = island_->get_position();
        auto origin_coord = island_->critical_core_loc();
        int circ_center_x = (pos.x.as_integer() + origin_coord.x * 16 + 16) -
                            PLATFORM.screen().get_view().get_center().x;
        int circ_center_y = (pos.y.as_integer() + origin_coord.y * 16 + 16) -
                            PLATFORM.screen().get_view().get_center().y - 510;
        // Platform::fatal(stringify(circ_center_y).c_str());
        PLATFORM_EXTENSION(overlay_circle_effect,
                           circ_effect_radius_,
                           circ_center_x,
                           circ_center_y);
    }


    WorldScene::display();

    if (confetti_ and *confetti_) {
        for (auto& c : **confetti_) {
            Sprite spr_;
            spr_.set_priority(0);
            spr_.set_position({Fixnum(c.x_), Fixnum(c.y_)});
            spr_.set_size(Sprite::Size::w8_h8);
            spr_.set_mix({[&] {
                              switch (c.clr_) {
                              default:
                              case 0:
                                  return ColorConstant::spanish_crimson;
                              case 1:
                                  return custom_color(0xbdef84);
                              case 2:
                                  return custom_color(0x006bff);
                              }
                          }(),
                          255});
            const auto off = c.img_ - 102;
            spr_.set_texture_index(102 * 8 + off + c.anim_ * 4);
            PLATFORM.screen().draw(spr_);
        }
    }
}



void update_confetti(ConfettiBuffer& confetti, Time delta)
{
    const auto view = PLATFORM.screen().get_view().get_center();

    for (auto it = confetti.begin(); it not_eq confetti.end();) {

        auto& c = *it;

        c.kf_++;
        if (c.kf_ == 5) {
            c.kf_ = 0;
            c.anim_ = !c.anim_;
        }

        if (c.y_ > view.y + 170) {
            it = confetti.erase(it);
        } else {
            auto step_vec = rotate({1, 0}, -c.angle_);
            step_vec.x *= c.speed_;
            step_vec.y *= c.speed_;
            c.x_ += step_vec.x * delta;
            c.y_ += step_vec.y * delta;

            c.speed_ = interpolate(0.000004f, c.speed_, 0.0000025f * delta);

            c.y_ += c.gravity_ * delta;

            if (c.fall_slower_) {
                c.gravity_ =
                    interpolate(0.00075f, c.gravity_, 0.000000015f * delta);
            } else {
                c.gravity_ =
                    interpolate(0.001f, c.gravity_, 0.000000015f * delta);
            }


            ++it;
        }
    }
}



ColorConstant
redden_shader(ShaderPalette p, ColorConstant k, int var, int index)
{
    if (p == ShaderPalette::overlay) {
        // Do not apply the redden effect to the overlay.
        return k;
    }

    auto k1 = contrast_shader(p, k, std::max(-(var / 2), -64), index);

    static constexpr const Color ao(ColorConstant::aerospace_orange);
    const Color input(k1);

    Color result(fast_interpolate(ao.r_, input.r_, var),
                 fast_interpolate(ao.g_, input.g_, var),
                 fast_interpolate(ao.b_, input.b_, var));


    return result.hex();
}



ScenePtr PlayerIslandDestroyedScene::update(Time delta)
{
    WorldScene::update(delta);

    if (APP.game_speed() not_eq GameSpeed::normal) {
        reset_gamespeed();
    }

    APP.environment().update(delta);

    APP.time_stream().enable_pushes(false);
    APP.time_stream().clear();


    APP.player().update(delta);


    const bool opponent_defeated =
        island_ not_eq &APP.player_island() and not forced_defeat_;


    const bool endgame =
        APP.game_mode() == App::GameMode::adventure and
        ((not opponent_defeated and
          APP.player_island().power_supply()) // surrender condition
         or (opponent_defeated and
             APP.world_graph().nodes_[APP.current_world_location()].type_ ==
                 WorldGraph::Node::Type::corrupted));


    if (confetti_ and *confetti_) {
        update_confetti(**confetti_, delta);
    }


    auto setup_shader = [] {
        PLATFORM.screen().set_shader(
            [](ShaderPalette p, ColorConstant k, int var, int index) {
                auto k1 = APP.environment().shader()(std::move(p),
                                                     std::move(k),
                                                     std::move(var),
                                                     std::move(index));
                return redden_shader(p, k1, var, index);
            });
    };


    auto pos = island_->get_position();
    if (pos.y < 600.0_fixed) {
        pos.y += sink_speed_ * APP.delta_fp();

        // Now, because some platforms implement automatic background wrapping
        // in hardware, we need to clear out tiles as they scroll outside the
        // view of the screen. On platforms where background mapping doesn't
        // matter, there's no harm in zeroing tiles as they scroll offscreen.

        // I originally forked the code from another of my projects as a
        // starting point for SKYLAND. BlindJump used larger tilemaps, so none
        // of this wrapping mattered early in development. Later, when I was
        // certain that SKYLAND would not use maps larger than 16x16, I decided
        // to free up some VRAM by making the background tile maps smaller. I
        // ended up throwing in this simple hack (below) as the most
        // straightforward way to deal with the wrapping issue. The only time
        // that a layer scrolls into wrapping territory is when a player
        // destroys an island, so I haven't bothered to clean this code up.

        const auto layer = island_->layer();

        for (int y = 15; y > 0; --y) {
            const auto y_pos = pos.y + Fixnum::from_integer(y * 16);
            if (y_pos > 700.0_fixed) {
                if (island_->flag_pos() and island_->flag_pos()->y >= y) {
                    island_->show_flag(false);
                }
                for (int x = 0; x < 16; ++x) {
                    PLATFORM.set_tile(layer, x, y, 0);
                    if (island_->fire_present({(u8)x, (u8)y})) {
                        island_->fire_extinguish({(u8)x, (u8)y});
                    }
                }
                island_->clear_rooms();
            }
        }
    }
    island_->set_position(pos);


    auto origin = island_->visual_origin();
    origin.x += Fixnum::from_integer(16 * (island_->terrain().size() / 2));

    const auto prev_timer = timer_;
    timer_ += delta;

    if (stat_timer_) {
        stat_timer_ -= delta;

        if (stat_timer_ < 0) {
            if (lines_.size() not_eq 4) {
                stat_timer_ = milliseconds(75 - lines_.size() * 5);
            } else {
                stat_timer_ = 0;
            }
            show_stats();
        }
    }

    static const auto music_fadeback_seconds = seconds(4);

    if (music_fadeback_timer_ > 0) {
        music_fadeback_timer_ -= delta;
        auto amount = interpolate(
            1, 12, Float(music_fadeback_timer_) / music_fadeback_seconds);
        if (not endgame) {
            PLATFORM.speaker().set_music_volume(amount);
        }
    }

    switch (anim_state_) {
    case AnimState::init: {
        PLATFORM.speaker().clear_sounds();
        if (not endgame) {
            PLATFORM.speaker().set_music_volume(1);
        }

        APP.effects().clear();

        if (((APP.game_mode() not_eq App::GameMode::adventure or endgame) and
             APP.game_mode() not_eq App::GameMode::skyland_forever and
             APP.game_mode() not_eq App::GameMode::co_op)) {
            PLATFORM.speaker().stream_music("unaccompanied_wind", 0);
        }

        PLATFORM.speaker().play_sound("explosion1", 3);

        big_explosion(origin, BigExplosionConfig{.draw_priority_ = 0});

        const auto off = 50.0_fixed;

        if (not opponent_defeated) {
            APP.swap_player<PlayerP1>();
        } else if (opponent_defeated and
                   APP.game_mode() == App::GameMode::sandbox) {
            // In case we were in spectator mode.
            APP.swap_player<PlayerP1>();
        }

        big_explosion({origin.x - off, origin.y - off},
                      BigExplosionConfig{.draw_priority_ = 0});

        big_explosion({origin.x + off, origin.y + off},
                      BigExplosionConfig{.draw_priority_ = 0});

        timer_ = 0;

        for (auto& room : APP.player_island().rooms()) {
            room->unset_target();
        }

        anim_state_ = AnimState::explosion_wait1;
        break;
    }

    case AnimState::explosion_wait1:

        if (timer_ > milliseconds(300)) {

            PLATFORM.speaker().play_sound("explosion1", 3);

            big_explosion(origin, BigExplosionConfig{.draw_priority_ = 0});
            const auto off = Fixnum::from_integer(-50);

            big_explosion({origin.x - off, origin.y + off},
                          BigExplosionConfig{.draw_priority_ = 0});

            big_explosion({origin.x + off, origin.y - off},
                          BigExplosionConfig{.draw_priority_ = 0});

            anim_state_ = AnimState::explosion_wait2;

            timer_ = 0;
            music_fadeback_timer_ = music_fadeback_seconds;
        }
        break;

    case AnimState::explosion_wait2: {
        last_radius_ = circ_effect_radius_;
        circ_effect_radius_ = ease_in(timer_, 0, 140, milliseconds(120));
        smoothstep(0.f, milliseconds(120), timer_);

        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::begin_fade;

            APP.rumble().activate(milliseconds(190));

            PLATFORM.speaker().play_sound("explosion2", 4);

            // If we destroyed the other island, erase all of the goblins on our
            // own island. We're doing this here, because the screen's faded to
            // white anyway, so it won't look so bad if the characters just
            // disappear.
            if (opponent_defeated) {

                invoke_hook("on-victory");
                if (APP.exit_condition() not_eq App::ExitCondition::none) {
                    if (APP.exit_condition() == App::ExitCondition::defeat) {
                        forced_defeat_ = true;
                    }
                }

                for (auto& room : APP.player_island().rooms()) {
                    for (auto it = room->characters().begin();
                         it not_eq room->characters().end();) {
                        if ((*it)->owner() not_eq &APP.player()) {
                            it = room->edit_characters().erase(it);
                        } else {
                            ++it;
                        }
                    }
                }

                Buffer<Transporter*, 16> ready_transporters;
                for (auto& room : APP.player_island().rooms()) {
                    if (auto tx = room->cast<Transporter>()) {
                        if (not tx->is_powered_down() and tx->ready()) {
                            ready_transporters.push_back(tx);
                        }
                    }
                }


                if (not ready_transporters.empty()) {
                    for (auto& room : APP.opponent_island()->rooms()) {
                        Buffer<RoomCoord, 8> temp;

                        for (auto& chr : room->characters()) {
                            if (chr->owner() == &APP.player()) {
                                temp.push_back(chr->grid_position());
                            }
                        }

                        for (auto pos : temp) {
                            if (not ready_transporters.empty()) {
                                auto tx = ready_transporters.back();
                                tx->recover_character(pos);
                                ready_transporters.pop_back();
                            }
                        }
                    }
                }

                for (auto& room : APP.player_island().rooms()) {
                    for (auto& chr : room->characters()) {
                        chr->unpin();
                    }
                }

                if (APP.game_mode() == App::GameMode::co_op) {
                    network::packet::CoOpOpponentDestroyed packet;
                    network::transmit(packet);
                }

            } else {
                APP.swap_opponent<FriendlyAI>();
            }

            for (auto& room : APP.player_island().rooms()) {
                room->detach_drone(true);
            }

            for (auto& room : APP.opponent_island()->rooms()) {
                room->detach_drone(true);
            }

            APP.player_island().drones().clear();
            APP.opponent_island()->drones().clear();

            APP.birds().clear(); // bugfix?

            for (u8 x = 0; x < 16; ++x) {
                for (u8 y = 0; y < 16; ++y) {
                    APP.player_island().fire_extinguish({x, y});
                    APP.opponent_island()->fire_extinguish({x, y});
                }
            }
        }
        break;
    }

    case AnimState::begin_fade: {
        anim_state_ = AnimState::begin_fade2;
        break;
    }

    case AnimState::begin_fade2: {
        anim_state_ = AnimState::fade;
        PLATFORM_EXTENSION(force_vsync);
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().fade(
            1.f, ColorConstant::silver_white, {}, true, true);

        auto origin_coord = island_->critical_core_loc();
        int circ_center_x = (pos.x.as_integer() + origin_coord.x * 16) + 8;
        int circ_center_y = (pos.y.as_integer() + origin_coord.y * 16) + 16;
        Vec2<Fixnum> pos;
        pos.x = circ_center_x;
        pos.y = circ_center_y;
        ExploSpawner::create(pos);
        break;
    }

    case AnimState::fade: {
        auto fade_duration = seconds(3) + milliseconds(500);

        circ_effect_radius_ = 0;

        if (prev_timer < seconds(1) and timer_ >= seconds(1)) {
            if (endgame) {
                PLATFORM.speaker().stream_music("music_box.raw", 0);
            }
        }

        sink_speed_ += 0.0000013_fixed;
        if (timer_ > fade_duration) {
            PLATFORM.screen().fade(0.f);
            timer_ = 0;
            if (forced_defeat_) {
                if (APP.dialog_buffer()) {
                    return make_dialog();
                }
            }
            if (opponent_defeated) {
                anim_state_ = AnimState::wait_1;
            } else {
                setup_shader();
                anim_state_ = AnimState::fade_out;
            }

        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(amount,
                                            ColorConstant::silver_white);
        }
        break;
    }

    case AnimState::wait_1: {

        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::show_coins;
            APP.set_coins(APP.coins() + APP.victory_coins());
        }
        break;
    }

    case AnimState::level_exit_forced:
        anim_state_ = AnimState::show_coins;
        timer_ = 0;

        if (endgame) {
            PLATFORM.speaker().stream_music("music_box.raw", 0);
        }

        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update();
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(achievement);

                PLATFORM.screen().fade(1.f);

                auto next = make_deferred_scene<PlayerIslandDestroyedScene>(
                    island_, true);
                return make_scene<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }

        setup_shader();

        for (auto& room : APP.player_island().rooms()) {
            room->detach_drone(true);
        }

        for (auto& room : APP.opponent_island()->rooms()) {
            room->detach_drone(true);
        }

        APP.player_island().drones().clear();
        APP.opponent_island()->drones().clear();

        for (auto& room : APP.player_island().rooms()) {
            room->unset_target();
        }

        if (is_player_island(island_)) {
            invoke_hook("on-victory");

            for (auto& room : APP.player_island().rooms()) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();) {
                    if ((*it)->owner() not_eq &APP.player()) {
                        it = room->edit_characters().erase(it);
                    } else {
                        ++it;
                    }
                }
            }
        }

        if (not endgame) {
            PLATFORM.speaker().set_music_volume(12);
        }

        handle_zone_exit();
        break;

    case AnimState::show_coins: {
        PLATFORM.speaker().play_sound("coin", 2);
        force_show_coins();
        APP.victory_coins() = 0;
        anim_state_ = AnimState::wait_2;
        break;
    }

    case AnimState::wait_2: {

        auto time = milliseconds(2000);

        if (timer_ > time) {
            timer_ = 0;
            coins_.reset();
            power_.reset();

            if (APP.game_mode() == App::GameMode::skyland_forever or
                APP.game_mode() == App::GameMode::co_op) {

                if (is_player_island(island_)) {
                    anim_state_ = AnimState::fade_out;
                    break;
                }

                APP.reset_opponent_island();


                if (APP.game_mode() == App::GameMode::co_op) {
                    for (auto& room : player_island().rooms()) {
                        // Just in case... should there be a bug in the locking
                        // anywhere (I haven't found any), unlock everything at
                        // the end of the level.
                        const bool was_locked = room->co_op_locked();
                        room->co_op_peer_release_lock();
                        if (was_locked and room->co_op_locked()) {
                            Platform::fatal("failed to release lock!");
                        } else if (was_locked) {
                            info("released lock!");
                        }

                        for (auto& chr : room->characters()) {
                            chr->co_op_release_lock();
                        }
                    }

                    return make_scene<CoOpSyncScene>();
                }

                return make_scene<ReadyScene>();

            } else {
                anim_state_ = AnimState::fade_out;
            }
        }
        break;
    }

    case AnimState::fade_out: {
        coins_.reset();
        power_.reset();

        if (timer_ - delta < milliseconds(600) and timer_ > milliseconds(600)) {
            PLATFORM.load_overlay_texture("overlay_island_destroyed");
            const int x_start = centered_text_margins(22);
            if (opponent_defeated) {
                draw_image(82, x_start, 1, 22, 8, Layer::overlay);
                confetti_state_ = ConfettiState::wait_1;
                confetti_timer_ = 0;
            } else {
                draw_image(259, x_start, 1, 22, 8, Layer::overlay);
            }
            stat_timer_ = milliseconds(145);
        }

        constexpr auto fade_duration = milliseconds(2800);
        if (timer_ > fade_duration) {
            timer_ = 0;
            anim_state_ = AnimState::idle;
        } else {
            const auto amount = smoothstep(0.f, fade_duration, timer_);
            if (not opponent_defeated) {
                PLATFORM.screen().set_shader_argument(amount * 200);
            }
            PLATFORM.screen().schedule_fade(amount * partial_fade_amt);
            PLATFORM.screen().pixelate(amount * 28, false);
        }
        break;
    }

    case AnimState::fade_complete: {
        coins_.reset();
        power_.reset();

        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {

            timer_ = fade_duration + 1;

            if (opponent_defeated) {

                handle_zone_exit();

                APP.reset_opponent_island();

                if (APP.game_mode() == App::GameMode::multiplayer and
                    PLATFORM.network_peer().is_connected()) {
                    PLATFORM.network_peer().disconnect();
                    return make_scene<LinkScene>();
                }

                switch (APP.game_mode()) {
                case App::GameMode::challenge:
                    return make_scene<LevelExitScene<SelectChallengeScene>>();

                case App::GameMode::adventure: {
                    if (APP.world_graph()
                            .nodes_[APP.current_world_location()]
                            .type_ == WorldGraph::Node::Type::corrupted) {

                        achievements::raise(achievements::Achievement::hero);

                        lisp::dostring("(adventure-log-add 6 '())");

                        auto dialog =
                            allocate_dynamic<DialogString>("dialog-buffer");
                        *dialog = SYS_CSTR(adventure_completed_message);
                        auto next =
                            make_scene<BoxedDialogScene>(std::move(dialog));

                        next->disallow_fastforward();

                        restore_volume_ = false;
                        if (not endgame) {
                            PLATFORM.speaker().set_music_volume(13);
                        }

                        next->set_next_scene([] {
                            auto next = make_scene<AdventureLogScene>();

                            next->set_next_scene([] {
                                auto next = make_scene<CrewStatsScene>(0);
                                next->next_ =
                                    make_deferred_scene<HighscoresScene>(true,
                                                                         1);
                                next->bkg_fade_amount_ = 1;
                                next->exit_fade_ = 1;
                                next->title_ =
                                    SystemString::crewmember_stats_title_ending;
                                return next;
                            });
                            return next;
                        });

                        return next;
                    } else {
                        return make_scene<LevelExitScene<ZoneImageScene>>();
                    }
                }

                case App::GameMode::sandbox:
                    return make_scene<LevelExitScene<SandboxResetScene>>();

                case App::GameMode::co_op:
                    Platform::fatal("logic error: co_op 111");

                case App::GameMode::skyland_forever:
                    return make_scene<ReadyScene>();

                case App::GameMode::multiplayer:
                    return make_scene<LinkScene>();

                default:
                    return make_scene<LevelExitScene<TitleScreenScene>>(3);
                }
                Platform::fatal("fallthrough in playerIslandDestroyedScene");
            } else {

                PLATFORM.screen().set_shader(APP.environment().shader());
                PLATFORM.screen().set_shader_argument(0);
                PLATFORM_EXTENSION(force_vsync);
                PLATFORM.screen().fade(1.f);
                hide_translucence();

                if (PLATFORM.network_peer().is_connected()) {
                    PLATFORM.network_peer().disconnect();
                    return make_scene<LevelExitScene<LinkScene>>();
                }

                switch (APP.game_mode()) {
                case App::GameMode::challenge:
                    return make_scene<LevelExitScene<SelectChallengeScene>>();

                case App::GameMode::adventure: {
                    lisp::dostring("(adventure-log-add 5 '())");
                    return make_scene<LevelExitScene<HighscoresScene>>(true, 1);
                }

                case App::GameMode::sandbox:
                    return make_scene<LevelExitScene<SandboxResetScene>>();

                case App::GameMode::multiplayer:
                    return make_scene<LevelExitScene<LinkScene>>();

                case App::GameMode::co_op:
                case App::GameMode::skyland_forever:
                    return make_scene<LevelExitScene<HighscoresScene>>(true, 3);

                default:
                    return make_scene<LevelExitScene<TitleScreenScene>>(3);
                }
            }
        } else {
            const auto amount =
                partial_fade_amt + (1.f - partial_fade_amt) *
                                       smoothstep(0.f, fade_duration, timer_);
            PLATFORM.screen().schedule_fade(amount);
        }
        break;
    }


    case AnimState::idle:
        coins_.reset();
        power_.reset();
        if (APP.player().key_down(Key::action_1) or
            APP.player().tap_released()) {
            timer_ = 0;
            PLATFORM.fill_overlay(0);

            if (opponent_defeated and
                APP.game_mode() == App::GameMode::adventure) {

                const auto node_type = APP.world_graph()
                                           .nodes_[APP.current_world_location()]
                                           .type_;

                if (node_type not_eq WorldGraph::Node::Type::exit and
                    node_type not_eq WorldGraph::Node::Type::corrupted and
                    options_allowed_) {
                    anim_state_ = AnimState::show_options;
                } else {
                    anim_state_ = AnimState::fade_complete;
                }

            } else {
                anim_state_ = AnimState::fade_complete;
            }
        }
        break;


    case AnimState::show_options:
        return make_scene<LevelCompleteOptionsScene>(false,
                                                     std::move(confetti_));
    }

    switch (confetti_state_) {
    case ConfettiState::dormant:
        break;

    case ConfettiState::wait_1:
        confetti_timer_ += delta;
        if (confetti_timer_ > milliseconds(500)) {
            confetti_timer_ = 0;
            confetti_state_ = ConfettiState::wait_2;

            auto vc = PLATFORM.screen().get_view().get_center();

            APP.camera()->shake(3);

            confetti_ = allocate_dynamic<ConfettiBuffer>("confetti");
            if (confetti_ and *confetti_) {
                for (int i = 0; i < 18; ++i) {

                    (*confetti_)
                        ->push_back(
                            {vc.x + 3,
                             vc.y + 140,
                             0.00008f *
                                 (1 + rng::choice<7>(rng::utility_state)),
                             20 + rng::choice<50>(rng::utility_state),
                             0.00000001f *
                                 (3 + rng::choice<600>(rng::utility_state)),
                             (u8)(102 + rng::choice<2>(rng::utility_state)),
                             (u8)(rng::choice<3>(rng::utility_state)),
                             0,
                             0,
                             (u8)(rng::choice<2>(rng::utility_state))});
                }
            }
        }
        break;

    case ConfettiState::confetti_pop_1:
        break;

    case ConfettiState::wait_2:
        confetti_timer_ += delta;
        if (confetti_timer_ > milliseconds(500)) {
            confetti_timer_ = 0;
            confetti_state_ = ConfettiState::wait_3;


            auto vc = PLATFORM.screen().get_view().get_center();

            APP.camera()->shake(3);

            for (int i = 0; i < 18; ++i) {
                (*confetti_)
                    ->push_back(
                        {vc.x + 235,
                         vc.y + 140,
                         0.00008f * (1 + rng::choice<7>(rng::utility_state)),
                         90 + 20 + rng::choice<50>(rng::utility_state),
                         0.0000009f,
                         (u8)(102 + rng::choice<4>(rng::utility_state)),
                         (u8)(rng::choice<3>(rng::utility_state)),
                         0,
                         0,
                         (u8)(rng::choice<2>(rng::utility_state))});
            }
        }
        break;

    case ConfettiState::confetti_pop_2:
        break;

    case ConfettiState::wait_3:
        // ...
        break;
    }

    return null_scene();
}



void PlayerIslandDestroyedScene::handle_zone_exit()
{
    if (APP.game_mode() == App::GameMode::adventure and
        APP.world_graph().nodes_[APP.current_world_location()].type_ ==
            WorldGraph::Node::Type::exit) {
        // We're at the exit node. Let's adjust stuff, so that we're at
        // the beginning of the next zone.
        APP.current_world_location() = 0;
        APP.world_graph().generate();
        APP.zone() += 1;

        if (APP.zone() == 4) {
            // No exit in the final zone, you have to fight the
            // storm king.
            for (auto& node : APP.world_graph().nodes_) {
                if (node.type_ == WorldGraph::Node::Type::exit) {
                    node.type_ = WorldGraph::Node::Type::hostile;
                }
            }
        }
    }
}



extern SharedVariable zone1_coin_yield;
extern SharedVariable zone2_coin_yield;
extern SharedVariable zone3_coin_yield;
extern SharedVariable zone4_coin_yield;



void PlayerIslandDestroyedScene::enter(Scene& prev)
{
    WorldScene::enter(prev);

    WorldScene::notransitions();

    rooms_built_ = APP.player().rooms_built_;
    rooms_lost_ = APP.player().rooms_lost_;

    disable_gamespeed_icon();

    for (auto& bird : APP.birds()) {
        bird->signal();
    }

    for (auto& room : APP.player_island().rooms()) {
        for (auto& chr : room->characters()) {
            chr->stats().info_.battles_fought_++;
        }
    }

    APP.persistent_data().total_seconds_.set(
        (u32)(APP.persistent_data().total_seconds_.get() +
              APP.stat_timer().whole_seconds()));

    level_seconds_ = APP.stat_timer().whole_seconds();

    if (APP.world_graph().nodes_[APP.current_world_location()].type_ ==
        WorldGraph::Node::Type::corrupted) {

        // At endgame, award the player score for any unused coins.

        int mult = 0.f;
        if (APP.zone() < 2) {
            mult = (0.01f * zone1_coin_yield);
        } else if (APP.zone() < 3) {
            mult = (0.01f * zone2_coin_yield);
        } else if (APP.zone() < 4) {
            mult = (0.01f * zone3_coin_yield);
        } else {
            mult = (0.01f * zone4_coin_yield);
        }

        APP.score().set(APP.score().get() + APP.coins() * mult);

        if (island_ not_eq &APP.player_island()) {

            auto add_score = 300000;

            if (not(APP.persistent_data().state_flags_.get() &
                    PersistentData::permadeath_on)) {
                add_score /= 2;
            }

            APP.persistent_data().score_.set(
                add_score + APP.persistent_data().score_.get());
        }
    }

    auto lv_score = APP.score().get() - APP.level_begin_score();
    auto score_time_penalty =
        0.5f * (lv_score - (lv_score / (std::max(1, level_seconds_ / 60))));
    APP.score().set(APP.score().get() - score_time_penalty);

    if (lv_score < 0) {
        APP.score().set(APP.level_begin_score());
    }

    for (auto& room : island_->rooms()) {
        if (room->position().y < 7) {
            // I expanded the maximum height of a castle, and now, when a
            // destroyed castle sinks, the castle can now be tall enough that
            // the upper portion sticks out above the bottom of the screen. So,
            // destroy all rooms with a really small y-value.
            room->apply_damage(Room::health_upper_limit());
        }
    }

    // Another patch for scroll wrapping as the destroyed castle falls. Rather
    // than deal with the issue, simply restrict the y range of the view, so the
    // player cannot see the portion of the screen where the tile layer wraps
    // around.
    globals().near_cursor_loc_.y =
        clamp(globals().near_cursor_loc_.y, (u8)9, (u8)14);
}



void PlayerIslandDestroyedScene::exit(Scene& next)
{
    lines_.clear();
    PLATFORM.load_overlay_texture("overlay");
    PLATFORM.screen().pixelate(0);

    PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::square_1);
    PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::square_2);
    PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::noise);
    PLATFORM_EXTENSION(psg_stop_note, Platform::Speaker::Channel::wave);

    if (restore_volume_) {
        PLATFORM.speaker().set_music_volume(
            Platform::Speaker::music_volume_max);
    }

    PLATFORM.screen().set_shader(APP.environment().shader());
}



} // namespace skyland
