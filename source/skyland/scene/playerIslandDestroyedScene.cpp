#include "playerIslandDestroyedScene.hpp"
#include "achievementNotificationScene.hpp"
#include "coopRngSyncScene.hpp"
#include "highscoresScene.hpp"
#include "levelCompleteOptionsScene.hpp"
#include "platform/color.hpp"
#include "readyScene.hpp"
#include "sandboxResetScene.hpp"
#include "scriptHookScene.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/network.hpp"
#include "skyland/player/opponent/friendlyAI.hpp"
#include "skyland/rooms/droneBay.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/serial.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



StringBuffer<32> format_time(u32 seconds, bool include_hours = true);



namespace skyland
{



static const Float partial_fade_amt = 0.76f;



void PlayerIslandDestroyedScene::show_stats(Platform& pfrm, App& app)
{
    const auto screen_tiles = calc_screen_tiles(pfrm);

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
            pfrm, Vec2<u8>{3, u8(metrics_y_offset_ + 8 + 2 * lines_.size())});

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
    case 0:
        if (app.game_mode() == App::GameMode::skyland_forever or
            app.game_mode() == App::GameMode::co_op) {
            print_metric_impl(
                SYSTR(level_complete_time)->c_str(),
                format_time(app.persistent_data().total_seconds_.get(), true));
        } else {
            print_metric_impl(
                SYSTR(level_complete_time)->c_str(),
                format_time(app.stat_timer().whole_seconds(), true));
        }
        break;

    case 1:
        if (app.game_mode() == App::GameMode::skyland_forever or
            app.game_mode() == App::GameMode::co_op) {
            print_metric(SYSTR(level_complete_pauses)->c_str(),
                         app.persistent_data().total_pauses_.get());
        } else {
            print_metric(SYSTR(level_complete_pauses)->c_str(),
                         app.pause_count());
        }
        break;

    case 2:
        print_metric(SYSTR(level_complete_coins)->c_str(),
                     app.level_coins_spent());
        break;

    case 3:
        StringBuffer<24> fmt;
        fmt += stringify(app.player().rooms_built_);
        fmt += "/";
        fmt += stringify(app.player().rooms_lost_);
        print_metric_impl(SYSTR(level_complete_rooms)->c_str(), fmt.c_str());
        break;
    }
}



void PlayerIslandDestroyedScene::display(Platform& pfrm, App& app)
{
    WorldScene::display(pfrm, app);

    if (confetti_ and *confetti_) {
        for (auto& c : **confetti_) {
            Sprite spr_;
            spr_.set_priority(0);
            spr_.set_position({c.x_, c.y_});
            spr_.set_size(Sprite::Size::w16_h32);
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
            spr_.set_texture_index(c.img_ + c.anim_ * 4);
            pfrm.screen().draw(spr_);
        }
    }
}



void update_confetti(Platform& pfrm,
                     App& app,
                     ConfettiBuffer& confetti,
                     Microseconds delta)
{
    const auto view = pfrm.screen().get_view().get_center();

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



ColorConstant redden_shader(int p, ColorConstant k, int var)
{
    if (p == 1) {
        // Do not apply the redden effect to the overlay.
        return k;
    }

    auto k1 = contrast_shader(p, k, std::max(-(var / 2), -64));

    static const Color ao(ColorConstant::aerospace_orange);
    const Color input(k1);

    Color result(fast_interpolate(ao.r_, input.r_, var),
                 fast_interpolate(ao.g_, input.g_, var),
                 fast_interpolate(ao.b_, input.b_, var));


    return result.hex();
}



ScenePtr<Scene>
PlayerIslandDestroyedScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    reset_gamespeed(pfrm, app);


    app.time_stream().enable_pushes(false);
    app.time_stream().clear();


    app.player().update(pfrm, app, delta);


    const bool opponent_defeated = island_ not_eq &app.player_island();


    if (confetti_ and *confetti_) {
        update_confetti(pfrm, app, **confetti_, delta);
    }


    auto pos = island_->get_position();
    if (pos.y < 600) {
        pos.y += sink_speed_ * delta;

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
            const auto y_pos = pos.y + y * 16;
            if (y_pos > 700) {
                if (island_->flag_pos() and island_->flag_pos()->y >= y) {
                    island_->show_flag(false);
                }
                for (int x = 0; x < 16; ++x) {
                    pfrm.set_tile(layer, x, y, 0);
                }
                island_->clear_rooms(pfrm, app);
            }
        }
    }
    island_->set_position(pos);


    auto origin = island_->visual_origin();
    origin.x += 16 * (island_->terrain().size() / 2);

    timer_ += delta;

    if (stat_timer_) {
        stat_timer_ -= delta;

        if (stat_timer_ < 0) {
            if (lines_.size() not_eq 4) {
                stat_timer_ = milliseconds(75 - lines_.size() * 5);
            } else {
                stat_timer_ = 0;
            }
            show_stats(pfrm, app);
        }
    }

    static const auto music_fadeback_seconds = seconds(4);

    if (music_fadeback_timer_ > 0) {
        music_fadeback_timer_ -= delta;
        auto amount = interpolate(
            1, 12, Float(music_fadeback_timer_) / music_fadeback_seconds);
        pfrm.speaker().set_music_volume(amount);
    }

    switch (anim_state_) {
    case AnimState::init: {
        pfrm.speaker().clear_sounds();
        pfrm.speaker().set_music_volume(1);

        if ((app.game_mode() not_eq App::GameMode::adventure and
             app.game_mode() not_eq App::GameMode::skyland_forever and
             app.game_mode() not_eq App::GameMode::co_op)) {
            pfrm.speaker().play_music("unaccompanied_wind", 0);
        }

        pfrm.speaker().play_sound("explosion1", 3);

        big_explosion(pfrm, app, origin);

        const auto off = 50.f;

        big_explosion(pfrm, app, {origin.x - off, origin.y - off});
        big_explosion(pfrm, app, {origin.x + off, origin.y + off});
        timer_ = 0;

        for (auto& room : app.player_island().rooms()) {
            room->unset_target(pfrm, app);
        }

        anim_state_ = AnimState::explosion_wait1;
        break;
    }

    case AnimState::explosion_wait1:
        if (timer_ > milliseconds(300)) {

            pfrm.speaker().play_sound("explosion1", 3);

            big_explosion(pfrm, app, origin);
            const auto off = -50.f;

            big_explosion(pfrm, app, {origin.x - off, origin.y + off});
            big_explosion(pfrm, app, {origin.x + off, origin.y - off});

            anim_state_ = AnimState::explosion_wait2;

            timer_ = 0;
            music_fadeback_timer_ = music_fadeback_seconds;
        }
        break;

    case AnimState::explosion_wait2:
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::fade;
            app.rumble().activate(pfrm, milliseconds(190));

            pfrm.speaker().play_sound("explosion2", 4);

            // If we destroyed the other island, erase all of the goblins on our
            // own island. We're doing this here, because the screen's faded to
            // white anyway, so it won't look so bad if the characters just
            // disappear.
            if (opponent_defeated) {

                invoke_hook(pfrm, app, "on-victory");

                for (auto& room : app.player_island().rooms()) {
                    for (auto it = room->characters().begin();
                         it not_eq room->characters().end();) {
                        if ((*it)->owner() not_eq &app.player()) {
                            it = room->characters().erase(it);
                        } else {
                            ++it;
                        }
                    }
                }
            } else {
                app.swap_opponent<FriendlyAI>();
            }

            for (auto& room : app.player_island().rooms()) {
                room->detach_drone(pfrm, app, true);
            }

            for (auto& room : app.opponent_island()->rooms()) {
                room->detach_drone(pfrm, app, true);
            }

            app.player_island().drones().clear();
            app.opponent_island()->drones().clear();
        }
        break;

    case AnimState::fade: {
        constexpr auto fade_duration = seconds(3) + milliseconds(500);

        sink_speed_ += 0.0000013f;
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            timer_ = 0;
            if (opponent_defeated) {
                anim_state_ = AnimState::wait_1;
            } else {
                pfrm.screen().set_shader(redden_shader);
                anim_state_ = AnimState::fade_out;
            }

        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(amount, ColorConstant::silver_white);
        }
        break;
    }

    case AnimState::wait_1: {
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::show_coins;
            app.set_coins(pfrm, app.coins() + app.victory_coins());
        }
        break;
    }

    case AnimState::level_exit_forced:
        anim_state_ = AnimState::show_coins;
        timer_ = 0;

        for (int i = 0; i < 64; ++i) {
            const auto achievement = achievements::update(pfrm, app);
            if (achievement not_eq achievements::Achievement::none) {
                achievements::award(pfrm, app, achievement);

                pfrm.screen().fade(1.f);

                auto next =
                    scene_pool::make_deferred_scene<PlayerIslandDestroyedScene>(
                        island_, true);
                return scene_pool::alloc<AchievementNotificationScene>(
                    achievement, next, true);
            }
        }


        pfrm.screen().set_shader(redden_shader);

        for (auto& room : app.player_island().rooms()) {
            room->detach_drone(pfrm, app, true);
        }

        for (auto& room : app.opponent_island()->rooms()) {
            room->detach_drone(pfrm, app, true);
        }

        app.player_island().drones().clear();
        app.opponent_island()->drones().clear();

        for (auto& room : app.player_island().rooms()) {
            room->unset_target(pfrm, app);
        }

        if (island_ == &app.player_island()) {
            invoke_hook(pfrm, app, "on-victory");

            for (auto& room : app.player_island().rooms()) {
                for (auto it = room->characters().begin();
                     it not_eq room->characters().end();) {
                    if ((*it)->owner() not_eq &app.player()) {
                        it = room->characters().erase(it);
                    } else {
                        ++it;
                    }
                }
            }
        }

        pfrm.speaker().set_music_volume(12);
        break;

    case AnimState::show_coins: {
        pfrm.speaker().play_sound("coin", 2);
        force_show_coins();
        app.victory_coins() = 0;
        anim_state_ = AnimState::wait_2;
        break;
    }

    case AnimState::wait_2: {
        if (timer_ > milliseconds(2000)) {
            timer_ = 0;
            coins_.reset();
            power_.reset();

            if (app.game_mode() == App::GameMode::skyland_forever or
                app.game_mode() == App::GameMode::co_op) {
                app.reset_opponent_island(pfrm);
                return scene_pool::alloc<ReadyScene>();
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
            pfrm.load_overlay_texture("overlay_island_destroyed");
            const int x_start = centered_text_margins(pfrm, 22);
            if (opponent_defeated) {
                draw_image(pfrm, 82, x_start, 1, 22, 8, Layer::overlay);
                confetti_state_ = ConfettiState::wait_1;
                confetti_timer_ = 0;
            } else {
                draw_image(pfrm, 259, x_start, 1, 22, 8, Layer::overlay);
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
                pfrm.screen().set_shader_argument(amount * 200);
            }
            pfrm.screen().schedule_fade(amount * partial_fade_amt);
            pfrm.screen().pixelate(amount * 28, false);
        }
        break;
    }

    case AnimState::fade_complete: {
        coins_.reset();
        power_.reset();

        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            if (opponent_defeated) {
                if (app.world_graph()
                        .nodes_[app.current_world_location()]
                        .type_ == WorldGraph::Node::Type::exit) {
                    // We're at the exit node. Let's adjust stuff, so that we're at
                    // the beginning of the next zone.
                    app.current_world_location() = 0;
                    app.world_graph().generate();
                    app.zone() += 1;
                }

                app.reset_opponent_island(pfrm);

                if (pfrm.network_peer().is_connected()) {
                    pfrm.network_peer().disconnect();
                    return scene_pool::alloc<TitleScreenScene>();
                }

                switch (app.game_mode()) {
                case App::GameMode::challenge:
                    return scene_pool::alloc<SelectChallengeScene>();

                case App::GameMode::adventure:
                    if (app.world_graph()
                            .nodes_[app.current_world_location()]
                            .type_ == WorldGraph::Node::Type::corrupted) {
                        // Defeated the storm king!
                        app.persistent_data().score_.set(
                            20000 + app.persistent_data().score_.get());
                        return scene_pool::alloc<HighscoresScene>(true, 1);
                    } else {
                        return scene_pool::alloc<ZoneImageScene>();
                    }

                case App::GameMode::sandbox:
                    return scene_pool::alloc<SandboxResetScene>();

                case App::GameMode::co_op:
                    return scene_pool::alloc<CoopRngSyncScene>();

                case App::GameMode::skyland_forever:
                    return scene_pool::alloc<ReadyScene>();

                case App::GameMode::multiplayer:
                    // TODO: default to the multiplayer page of the Title
                    // screen.
                    return scene_pool::alloc<TitleScreenScene>();

                default:
                    return scene_pool::alloc<TitleScreenScene>(3);
                }
                Platform::fatal("fallthrough in playerIslandDestroyedScene");
            } else {

                pfrm.screen().set_shader(passthrough_shader);
                pfrm.screen().set_shader_argument(0);
                pfrm.system_call("vsync", nullptr);
                pfrm.screen().fade(1.f);

                if (pfrm.network_peer().is_connected()) {
                    pfrm.network_peer().disconnect();
                    return scene_pool::alloc<TitleScreenScene>();
                }

                switch (app.game_mode()) {
                case App::GameMode::challenge:
                    return scene_pool::alloc<SelectChallengeScene>();

                case App::GameMode::adventure:
                    return scene_pool::alloc<HighscoresScene>(true, 1);

                case App::GameMode::sandbox:
                    return scene_pool::alloc<SandboxResetScene>();

                case App::GameMode::multiplayer:
                    // TODO: default to the multiplayer page of the Title
                    // screen.
                    return scene_pool::alloc<TitleScreenScene>();

                case App::GameMode::co_op:
                case App::GameMode::skyland_forever:
                    return scene_pool::alloc<HighscoresScene>(true, 3);

                default:
                    return scene_pool::alloc<TitleScreenScene>(3);
                }
            }
        } else {
            const auto amount =
                partial_fade_amt + (1.f - partial_fade_amt) *
                                       smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().schedule_fade(amount);
        }
        break;
    }


    case AnimState::idle:
        coins_.reset();
        power_.reset();
        if (app.player().key_down(pfrm, Key::action_1) or
            app.player().tap_released(pfrm)) {
            timer_ = 0;
            lines_.clear();
            pfrm.fill_overlay(0);
            app.reset_opponent_island(pfrm);

            if (opponent_defeated and
                app.game_mode() == App::GameMode::adventure) {

                const auto node_type = app.world_graph()
                                           .nodes_[app.current_world_location()]
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
        return scene_pool::alloc<LevelCompleteOptionsScene>(
            false, std::move(confetti_));
    }

    switch (confetti_state_) {
    case ConfettiState::dormant:
        break;

    case ConfettiState::wait_1:
        confetti_timer_ += delta;
        if (confetti_timer_ > milliseconds(500)) {
            confetti_timer_ = 0;
            confetti_state_ = ConfettiState::wait_2;

            auto vc = pfrm.screen().get_view().get_center();

            app.camera()->shake(3);

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


            auto vc = pfrm.screen().get_view().get_center();

            app.camera()->shake(3);

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



void PlayerIslandDestroyedScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    WorldScene::enter(pfrm, app, prev);

    app.persistent_data().total_seconds_.set(
        (u32)(app.persistent_data().total_seconds_.get() +
              app.stat_timer().whole_seconds()));

    app.persistent_data().total_pauses_.set(
        app.persistent_data().total_pauses_.get() + app.pause_count());

    for (auto& room : island_->rooms()) {
        if (room->position().y < 7) {
            // I expanded the maximum height of a castle, and now, when a
            // destroyed castle sinks, the castle can now be tall enough that
            // the upper portion sticks out above the bottom of the screen. So,
            // destroy all rooms with a really small y-value.
            room->apply_damage(pfrm, app, Room::health_upper_limit());
        }
    }

    // Another patch for scroll wrapping as the destroyed castle falls. Rather
    // than deal with the issue, simply restrict the y range of the view, so the
    // player cannot see the portion of the screen where the tile layer wraps
    // around.
    std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y =
        clamp(std::get<SkylandGlobalData>(globals()).near_cursor_loc_.y,
              (u8)9,
              (u8)14);
}



void PlayerIslandDestroyedScene::exit(Platform& pfrm, App& app, Scene& next)
{
    lines_.clear();
    pfrm.load_overlay_texture("overlay");
    pfrm.screen().pixelate(0);

    pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_1);
    pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::square_2);
    pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::noise);
    pfrm.speaker().stop_chiptune_note(Platform::Speaker::Channel::wave);

    pfrm.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    pfrm.screen().set_shader(passthrough_shader);
}



} // namespace skyland
