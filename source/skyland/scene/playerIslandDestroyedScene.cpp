#include "playerIslandDestroyedScene.hpp"
#include "highscoresScene.hpp"
#include "localization.hpp"
#include "selectChallengeScene.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/serial.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"
#include "zoneImageScene.hpp"



StringBuffer<32> format_time(u32 seconds, bool include_hours = true);



namespace skyland {



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
        print_metric_impl(str, to_string<20>(num), suffix, highlight);
    };

    switch (lines_.size()) {
    case 0:
        print_metric_impl("time ",
                          format_time(app.level_timer().whole_seconds(), true));
        break;

    case 1:
        print_metric("pauses used ", app.pause_count());
        break;

    case 2:
        print_metric("coins spent ", app.level_coins_spent());
        break;

    case 3:
        StringBuffer<24> fmt;
        fmt += to_string<10>(app.player().rooms_built_);
        fmt += "/";
        fmt += to_string<10>(app.player().rooms_lost_);
        print_metric_impl("rooms built/lost ", fmt.c_str());
        break;
    }
}



ScenePtr<Scene>
PlayerIslandDestroyedScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    WorldScene::update(pfrm, app, delta);

    app.paused() = false;

    auto pos = island_->get_position();
    if (pos.y < 700) {
        pos.y += sink_speed_ * delta;
    }
    island_->set_position(pos);


    auto origin = island_->origin();
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


    switch (anim_state_) {
    case AnimState::init: {
        pfrm.speaker().stop_music();

        big_explosion(pfrm, app, origin);

        const auto off = 50.f;

        big_explosion(pfrm, app, {origin.x - off, origin.y - off});
        big_explosion(pfrm, app, {origin.x + off, origin.y + off});
        timer_ = 0;

        for (auto& room : app.player_island().rooms()) {
            room->unset_target();
        }

        anim_state_ = AnimState::explosion_wait1;
        break;
    }

    case AnimState::explosion_wait1:
        if (timer_ > milliseconds(300)) {
            big_explosion(pfrm, app, origin);
            const auto off = -50.f;

            big_explosion(pfrm, app, {origin.x - off, origin.y + off});
            big_explosion(pfrm, app, {origin.x + off, origin.y - off});

            anim_state_ = AnimState::explosion_wait2;

            timer_ = 0;
        }
        break;

    case AnimState::explosion_wait2:
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::fade;
            app.rumble().activate(pfrm, milliseconds(190));

            // If we destroyed the other island, erase all of the goblins on our
            // own island. We're doing this here, because the screen's faded to
            // white anyway, so it won't look so bad if the characters just
            // disappear.
            if (island_ not_eq &app.player_island()) {
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
        }
        break;

    case AnimState::fade: {
        constexpr auto fade_duration = seconds(3) + milliseconds(500);
        // if (timer_ > seconds(1) + milliseconds(700) and
        //     not pushed_notification_) {
        //     pushed_notification_ = true;
        //     push_notification(
        //         pfrm, this, locale_string(pfrm, defeated_text_)->c_str());
        // }
        sink_speed_ += 0.0000013f;
        if (timer_ > fade_duration) {
            pfrm.screen().fade(0.f);
            timer_ = 0;
            if (island_ not_eq &app.player_island()) {
                anim_state_ = AnimState::wait_1;
            } else {
                anim_state_ = AnimState::fade_out;
            }

        } else {
            const auto amount = 1.f - smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(amount, ColorConstant::silver_white);
        }
        break;
    }

    case AnimState::wait_1: {
        if (timer_ > milliseconds(120)) {
            timer_ = 0;
            anim_state_ = AnimState::add_score;
        }
        break;
    }

    case AnimState::add_score: {
        app.coins() += app.victory_coins();
        app.victory_coins() = 0;
        anim_state_ = AnimState::wait_2;

        // info(pfrm, serialize(pfrm, app.player_island())->c_str());

        break;
    }

    case AnimState::wait_2: {
        if (timer_ > milliseconds(2000)) {
            timer_ = 0;
            anim_state_ = AnimState::fade_out;
            coins_.reset();
            power_.reset();
        }
        break;
    }

    case AnimState::fade_out: {
        coins_.reset();
        power_.reset();

        if (timer_ - delta < milliseconds(600) and timer_ > milliseconds(600)) {
            pfrm.load_overlay_texture("overlay_island_destroyed");
            if (island_ not_eq &app.player_island()) {
                draw_image(pfrm, 82, 4, 1, 22, 8, Layer::overlay);
            } else {
                draw_image(pfrm, 259, 4, 1, 22, 8, Layer::overlay);
            }
            stat_timer_ = milliseconds(145);
        }

        constexpr auto fade_duration = milliseconds(2800);
        if (timer_ > fade_duration) {
            timer_ = 0;
            anim_state_ = AnimState::idle;
        } else {
            const auto amount =
                smoothstep(0.f, fade_duration, timer_) * partial_fade_amt;
            pfrm.screen().fade(amount);
        }
        break;
    }

    case AnimState::fade_complete: {
        coins_.reset();
        power_.reset();

        constexpr auto fade_duration = milliseconds(350);
        if (timer_ > fade_duration) {
            if (island_ not_eq &app.player_island()) {
                if (app.current_map_location() == Vec2<u8>{7, 1}) {
                    // We're at the exit node. Let's adjust stuff, so that we're at
                    // the beginning of the next zone.
                    app.current_map_location().x = 0;
                    app.world_map().generate();
                    app.zone() += 1;
                }

                app.opponent_island().reset();

                app.persistent_data().total_seconds_.set(
                    (u32)(app.persistent_data().total_seconds_.get() +
                          app.level_timer().whole_seconds()));

                app.persistent_data().total_pauses_.set(
                    app.persistent_data().total_pauses_.get() +
                    app.pause_count());


                if (pfrm.network_peer().is_connected()) {
                    pfrm.network_peer().disconnect();
                    return scene_pool::alloc<TitleScreenScene>();
                }
                if (app.challenge_mode()) {
                    return scene_pool::alloc<SelectChallengeScene>();
                } else {
                    return scene_pool::alloc<ZoneImageScene>();
                }
            } else {
                if (pfrm.network_peer().is_connected()) {
                    pfrm.network_peer().disconnect();
                    return scene_pool::alloc<TitleScreenScene>();
                }
                if (app.challenge_mode()) {
                    return scene_pool::alloc<SelectChallengeScene>();
                } else {
                    return scene_pool::alloc<HighscoresScene>();
                }
            }
        } else {
            const auto amount =
                partial_fade_amt + (1.f - partial_fade_amt) *
                                       smoothstep(0.f, fade_duration, timer_);
            pfrm.screen().fade(amount);
        }
        break;
    }

    case AnimState::idle:
        coins_.reset();
        power_.reset();
        if (key_down<Key::action_1>(pfrm)) {
            timer_ = 0;
            lines_.clear();
            pfrm.fill_overlay(0);
            anim_state_ = AnimState::fade_complete;
        }
        break;
    }

    return null_scene();
}



void PlayerIslandDestroyedScene::exit(Platform& pfrm, App& app, Scene& next)
{
    lines_.clear();
    pfrm.load_overlay_texture("overlay");
}



} // namespace skyland
