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


#include "worldScene.hpp"
#include "achievementNotificationScene.hpp"
#include "boxedDialogScene.hpp"
#include "easyModeRewindScene.hpp"
#include "globals.hpp"
#include "multiplayerReadyScene.hpp"
#include "notificationScene.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "readyScene.hpp"
#include "scriptHookScene.hpp"
#include "selInputScene.hpp"
#include "setGamespeedScene.hpp"
#include "skyland/achievement.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birds/smallBird.hpp"
#include "skyland/scene/playerIslandDestroyedScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "surrenderWaitScene.hpp"



namespace skyland
{



static void apply_gamespeed(App& app, Microseconds& delta)
{
    switch (app.game_speed()) {
    case GameSpeed::normal:
        break;

    case GameSpeed::slow:
        delta /= 2;
        break;

    case GameSpeed::fast:
        delta *= 2;
        break;

    case GameSpeed::stopped:
        delta = 0;
        break;

    case GameSpeed::rewind:
        // NOTE: we shouldn't even be in this scene if we're rewinding.
        Platform::fatal("gamespeed set to rewind in incompatible scene.");
        break;

    case GameSpeed::count:
        // ... raise error?
        break;
    }
}



u16 gamespeed_icon(GameSpeed speed)
{
    switch (speed) {
    case GameSpeed::normal:
        return 358;

    case GameSpeed::slow:
        return 354;

    case GameSpeed::fast:
        return 362;

    case GameSpeed::stopped:
        return 177;

    case GameSpeed::rewind:
        return 366;

    default:
        break;
    }
    return 0;
}



static void show_multiplayer_pauses_remaining(Platform& pfrm)
{
    auto& g = std::get<SkylandGlobalData>(globals());
    auto st = calc_screen_tiles(pfrm);

    auto current = pfrm.get_tile(Layer::overlay, st.x - 3, 3);
    if (current == 410) {
        // Already assigned, skip integer->text conversion below.
        return;
    }

    pfrm.set_tile(Layer::overlay, st.x - 3, 3, 410);
    Text t(pfrm, OverlayCoord{u8(st.x - 2), 3});
    t.assign("x");
    t.append(g.multiplayer_pauses_remaining_);
    t.__detach();
}



static void hide_multiplayer_pauses_remaining(Platform& pfrm)
{
    auto st = calc_screen_tiles(pfrm);

    auto current = pfrm.get_tile(Layer::overlay, st.x - 3, 3);
    if (current == 0) {
        return;
    }

    pfrm.set_tile(Layer::overlay, st.x - 3, 3, 0);
    pfrm.set_tile(Layer::overlay, st.x - 2, 3, 0);
    pfrm.set_tile(Layer::overlay, st.x - 1, 3, 0);
}



void WorldScene::set_gamespeed(Platform& pfrm, App& app, GameSpeed speed)
{
    switch (speed) {
    case GameSpeed::stopped:
    case GameSpeed::normal:
        if (app.game_speed() not_eq GameSpeed::stopped and
            app.game_speed() not_eq GameSpeed::normal) {
            pfrm.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::regular);
        }
        break;

    case GameSpeed::fast:
        pfrm.speaker().set_music_speed(Platform::Speaker::MusicSpeed::doubled);
        break;

    case GameSpeed::rewind:
        pfrm.speaker().set_music_speed(Platform::Speaker::MusicSpeed::reversed);
        break;

    case GameSpeed::slow:
        pfrm.speaker().set_music_speed(Platform::Speaker::MusicSpeed::halved);
        break;

    case GameSpeed::count:
        // Erroneous enumeration.
        break;
    }

    if (speed == GameSpeed::stopped) {
        pfrm.speaker().stash_sounds();
    } else if (app.game_speed() == GameSpeed::stopped) {
        pfrm.speaker().restore_sounds();
    }

    app.game_speed() = speed;
    if (speed == GameSpeed::normal) {
        set_pause_icon(pfrm, 0);
    } else {
        set_pause_icon(pfrm, gamespeed_icon(speed));
    }
}



void WorldScene::reset_gamespeed(Platform& pfrm, App& app)
{
    set_gamespeed(pfrm, app, GameSpeed::normal);
}



ScenePtr<Scene>
ActiveWorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    app.player().update(pfrm, app, delta);

    app.opponent().update(pfrm, app, delta);


    if (auto new_scene = WorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }


    if (app.game_mode() == App::GameMode::adventure or
        app.game_mode() == App::GameMode::skyland_forever) {
        const auto achievement = achievements::update(pfrm, app);
        if (achievement not_eq achievements::Achievement::none) {
            achievements::award(pfrm, app, achievement);

            auto next = scene_pool::make_deferred_scene<ReadyScene>();

            return scene_pool::alloc<AchievementNotificationScene>(achievement,
                                                                   next);
        }
    }


    if (app.game_mode() not_eq App::GameMode::multiplayer and
        app.game_mode() not_eq App::GameMode::multiplayer and
        app.input_setup_info()) {

        auto next = scene_pool::alloc<SelInputScene>(*app.input_setup_info(),
                                                     not is_far_camera());

        app.input_setup_info().reset();

        return next;
    }


    apply_gamespeed(app, delta);


    app.environment().update(pfrm, app, delta);


    if (app.player_island().is_destroyed()) {

        if ((app.game_mode() == App::GameMode::adventure or
             app.game_mode() == App::GameMode::skyland_forever) and
            app.persistent_data().difficulty_ ==
                PersistentData::Difficulty::beginner and
            not state_bit_load(app, StateBit::easy_mode_rewind_declined)) {

            return scene_pool::alloc<EasyModeRewindScene>();
        }

        state_bit_store(app, StateBit::easy_mode_rewind_declined, false);

        reset_gamespeed(pfrm, app);

        app.time_stream().clear();

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        cursor_loc.x = 0;

        if (app.game_mode() not_eq App::GameMode::multiplayer and
            app.game_mode() not_eq App::GameMode::co_op) {
            pfrm.sleep(4);
        }

        app.effects().clear();
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            &app.player_island());
    }

    if (auto o = app.opponent_island()) {
        if (not state_bit_load(app, StateBit::surrender_offered) and
            app.game_mode() == App::GameMode::adventure) {
            if (not app.player_island().is_boarded() and
                o->offensive_capabilities() == 0 and o->character_count() and
                o->projectiles().empty()) {

                state_bit_store(app, StateBit::surrender_offered, true);

                // The final boss will never surrender.
                if (app.world_graph()
                        .nodes_[app.current_world_location()]
                        .type_ not_eq WorldGraph::Node::Type::corrupted) {

                    return scene_pool::alloc<SurrenderWaitScene>();
                }
            }
        }
    }

    if (app.opponent_island() and app.opponent_island()->is_destroyed()) {
        reset_gamespeed(pfrm, app);

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        cursor_loc.x = 0;

        app.effects().clear();
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            app.opponent_island());
    }

    return null_scene();
}



void WorldScene::display(Platform& pfrm, App& app)
{
    app.environment().display(pfrm, app);

    if (app.game_mode() == App::GameMode::co_op) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h32);
        cursor.set_texture_index(
            std::get<SkylandGlobalData>(globals()).co_op_cursor_icon_);

        auto cursor_loc = std::get<SkylandGlobalData>(globals()).co_op_cursor_;

        if (std::get<SkylandGlobalData>(globals()).co_op_cursor_near_) {

            auto origin = app.player_island().visual_origin();

            origin.x += cursor_loc.x * 16;
            origin.y += cursor_loc.y * 16;

            cursor.set_position(origin);
        } else if (app.opponent_island()) {
            auto origin = app.opponent_island()->visual_origin();

            origin.x += cursor_loc.x * 16;
            origin.y += cursor_loc.y * 16;

            cursor.set_position(origin);
        }

        pfrm.screen().draw(cursor);
    }

    app.player_island().display(pfrm);

    if (app.opponent_island()) {
        app.opponent_island()->display(pfrm);
    }

    for (auto& effect : app.effects()) {
        pfrm.screen().draw(effect->sprite());
    }

    if (not birds_drawn_) {
        // We try to queue bird entities before a display call, because unlike
        // other objects, birds sit directly on top of tiles, and if there's any
        // tearing whatsoever, it's super noticeable. But not all derived scenes
        // actually call WorldScene::update(), so we perform the draw call here
        // for those cases.
        for (auto& bird : app.birds()) {
            pfrm.screen().draw(bird->sprite());
        }
    }
    birds_drawn_ = false;
}



static u32 format_power_fraction(Power avail, Power used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



bool WorldScene::camera_update_check_key(Platform& pfrm, App& app)
{
    return app.player().key_pressed(pfrm, Key::left) or
           app.player().key_pressed(pfrm, Key::right) or
           app.player().key_pressed(pfrm, Key::up) or
           app.player().key_pressed(pfrm, Key::down) or
           app.player().key_pressed(pfrm, Key::select);
}



ScenePtr<Scene> WorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    auto& g = std::get<SkylandGlobalData>(globals());


    if (not pfrm.network_peer().is_connected()) {
        // We scale game updates based on frame delta. But if the game starts to
        // lag a lot, the logic can start to get screwed up, so at some point,
        // the player will in fact start to notice the lag.
        delta = std::min(delta, seconds(1) / 18);
    }


    Microseconds world_delta = delta;

    if (not pfrm.network_peer().is_connected()) {
        // NOTE: we can't clamp the clock delta in multiplayer modes of course!

        // The game uses clock deltas for update logic throughout the code, but
        // huge deltas mess up the collision checking, so in certain cases, it's
        // better to just clamp the delta time and allow some lag.
        //
        // i.e.: lag the game if it lags slower than 30fps.
        world_delta = clamp(world_delta, seconds(0), seconds(1) / 30);
    }

    apply_gamespeed(app, world_delta);

    app.delta_fp() = world_delta;

    app.update_parallax(world_delta);


    app.stat_timer().count_up(delta);
    app.level_timer().count_up(world_delta);
    app.time_stream().update(world_delta);


    auto& mt_prep_timer = g.multiplayer_prep_timer_;

    auto& mt_prep_seconds = g.multiplayer_prep_seconds_;


    if (pfrm.network_peer().is_connected()) {
        if (mt_prep_seconds) {
            if (app.game_speed() not_eq GameSpeed::stopped) {
                mt_prep_timer += delta;
                if (mt_prep_timer > seconds(1)) {
                    mt_prep_timer -= seconds(1);
                    mt_prep_seconds--;

                    if (mt_prep_seconds == 0 and
                        app.game_mode() not_eq App::GameMode::co_op) {
                        return scene_pool::alloc<MultiplayerReadyScene>();
                    }

                    StringBuffer<30> msg = "get ready! 0";
                    msg += stringify(mt_prep_seconds / 60);
                    msg += ":";
                    const auto rem = mt_prep_seconds % 60;
                    if (rem < 10) {
                        msg += "0";
                    }
                    msg += stringify(rem);

                    const u8 margin = centered_text_margins(pfrm, msg.length());


                    g.multiplayer_prep_text_.emplace(
                        pfrm, msg.c_str(), OverlayCoord{margin, 4});
                }
            }
        } else {
            g.multiplayer_prep_text_.reset();
        }
    } else {
        g.multiplayer_prep_text_.reset();
    }


    auto tapped_topright_corner = [&] {
        if (auto pos = app.player().tap_released(pfrm)) {
            auto sz = pfrm.screen().size();
            if (pos->x > sz.x - 36 and pos->y < 36) {
                return true;
            }
        }
        return false;
    };

    auto ret = null_scene();


    if (app.game_mode() == App::GameMode::multiplayer) {
        // TODO... currently unsupported
        set_gamespeed(pfrm, app, GameSpeed::normal);
    } else if (not noreturn_ and (app.player().key_up(pfrm, Key::alt_1) or
                                  tapped_topright_corner())) {
        if (app.game_speed() not_eq GameSpeed::stopped) {

            bool can_pause = true;

            if (pfrm.network_peer().is_connected()) {
                if (not g.multiplayer_pauses_remaining_) {
                    can_pause = false;
                    pfrm.speaker().play_sound("beep_error", 3);
                    set_gamespeed(pfrm, app, GameSpeed::normal);
                    auto future_scene = []() {
                        return scene_pool::alloc<ReadyScene>();
                    };
                    auto str = SYSTR(error_no_more_pauses);
                    ret = scene_pool::alloc<NotificationScene>(str->c_str(),
                                                               future_scene);
                } else {
                    g.multiplayer_pause_owner_ = true;
                    g.multiplayer_pauses_remaining_--;
                }
            }

            if (can_pause) {
                app.pause_count()++;
                set_gamespeed(pfrm, app, GameSpeed::stopped);

                if (pfrm.network_peer().is_connected()) {
                    network::packet::Paused pkt;
                    pkt.status_ = true;
                    network::transmit(pfrm, pkt);
                }
            }

        } else {

            bool can_unpause = true;

            if (pfrm.network_peer().is_connected()) {
                if (not g.multiplayer_pause_owner_) {
                    can_unpause = false;
                    pfrm.speaker().play_sound("beep_error", 3);
                } else {
                    g.multiplayer_pause_owner_ = false;
                }
            }

            if (can_unpause) {
                if (pfrm.network_peer().is_connected()) {
                    network::packet::Paused pkt;
                    pkt.status_ = false;
                    network::transmit(pfrm, pkt);
                }

                set_gamespeed(pfrm, app, GameSpeed::normal);
            }
        }
        app.player().touch_consume();
    } else if (not noreturn_ and app.player().key_pressed(pfrm, Key::alt_1) and
               not pfrm.network_peer().is_connected()) {
        set_gamespeed_keyheld_timer_ += delta;
        if (set_gamespeed_keyheld_timer_ > milliseconds(300)) {
            return scene_pool::alloc<SetGamespeedScene>();
        }
    } else {
        set_gamespeed_keyheld_timer_ = 0;
    }

    if (app.opponent_island()) {

        const bool show_opponent_interior =
            (app.player_island().has_radar() and
             not static_cast<Opponent&>(app.opponent_island()->owner())
                     .is_friendly()) or
            app.opponent_island()->is_boarded() or
            (app.player_island().interior_visible() and
             app.game_mode() == App::GameMode::sandbox);

        if (not app.opponent_island()->interior_visible() and
            show_opponent_interior) {

            show_island_interior(pfrm, app, app.opponent_island());

        } else if (app.opponent_island()->interior_visible() and
                   not show_opponent_interior) {

            show_island_exterior(pfrm, app, app.opponent_island());

            app.opponent_island()->render_exterior(pfrm, app);
        }

        // Hey, I threw this code together in a panic for a game jam, I know
        // this is illegible. Drift opponent island toward the player, until
        // a certain distance. If the player extends the terrain on his own
        // island, drift the opponent island away to maintain the ideal
        // distance between the two.
        if ((app.opponent_island()->get_drift() < 0 and
             app.opponent_island()->get_position().x.as_integer() <=
                 (int)app.player_island().terrain().size() * 16 + 48) or
            (app.opponent_island()->get_drift() > 0 and
             app.opponent_island()->get_position().x.as_integer() >
                 (int)app.player_island().terrain().size() * 16 + 48)) {

            app.opponent_island()->set_position(
                {(Float)app.player_island().terrain().size() * 16 + 48,
                 app.opponent_island()->get_position().y});
            app.opponent_island()->set_drift(pfrm, app, 0);

            app.on_timeout(
                pfrm, milliseconds(500), [](Platform& pfrm, App& app) {
                    invoke_hook(pfrm, app, "on-converge");
                });
        }

        if (app.opponent_island()->get_drift() == 0) {
            if (app.opponent_island()->get_position().x.as_integer() <
                (int)app.player_island().terrain().size() * 16 + 48) {
                app.opponent_island()->set_drift(pfrm, app, 0.00003f);
            }
        }
    }

    if (camera_update_check_key(pfrm, app)) {
        camera_update_timer_ = milliseconds(500);
    }

    if (app.camera()->is_shaking() or camera_update_timer_ > 0 or
        app.player().touch_current(pfrm) or app.camera()->always_update(pfrm)) {

        camera_update_timer_ -= delta;
        camera_update_timer_ = std::max((int)camera_update_timer_, 0);

        // You may be wondering, why are we setting a timer to determine whether
        // to update the camera? Because we're using a floating point value
        // multiplied by a delta time for linear interpolation, for really
        // fine-grained camera movements, the camera target can get stuck
        // flipping back and forth over a fractional pixel. Not updating the
        // camera for times longer then half a second makes the issue go
        // away. It really only happens when the camera's sitting idle in the
        // same spot for a long time.

        if (app.opponent_island() and UNLIKELY(far_camera_)) {
            auto& cursor_loc = g.far_cursor_loc_;
            app.camera()->update(
                pfrm, app, *app.opponent_island(), cursor_loc, delta, false);
        } else {
            auto& cursor_loc = g.near_cursor_loc_;
            app.camera()->update(
                pfrm, app, app.player_island(), cursor_loc, delta, true);
        }
    }

    if (app.player().key_down(pfrm, Key::action_4)) {
        pfrm.system_call("swap-screens", nullptr);
    }


    if (app.player().key_down(pfrm, Key::select)) {
        if (app.player_island().interior_visible()) {
            show_island_exterior(pfrm, app, &app.player_island());
        } else {
            show_island_interior(pfrm, app, &app.player_island());
        }
    }

    if (not noreturn_ and app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();
        bool answer = state_bit_load(app, StateBit::dialog_expects_answer);
        state_bit_store(app, StateBit::dialog_expects_answer, false);
        return scene_pool::alloc<BoxedDialogSceneWS>(std::move(buffer), answer);
    }


    app.player_island().update(pfrm, app, world_delta);

    if (app.opponent_island()) {
        app.opponent_island()->update(pfrm, app, world_delta);
    }

    update_entities(pfrm, app, world_delta, app.effects());

    update_entities(pfrm, app, world_delta, app.birds());

    for (auto& bird : app.birds()) {
        pfrm.screen().draw(bird->sprite());
    }
    birds_drawn_ = true;


    if (app.game_speed() not_eq GameSpeed::stopped) {
        hide_multiplayer_pauses_remaining(pfrm);
    } else {
        set_pause_icon(pfrm, gamespeed_icon(app.game_speed()));

        if (pfrm.network_peer().is_connected()) {
            show_multiplayer_pauses_remaining(pfrm);
        }
    }


    Island* disp_power = power_fraction_opponent_island_
                             ? (app.opponent_island() ? app.opponent_island()
                                                      : &app.player_island())
                             : &app.player_island();

    if (last_power_supplied_ not_eq disp_power->power_supply() or
        last_power_used_ not_eq disp_power->power_drain()) {

        last_power_supplied_ = disp_power->power_supply();
        last_power_used_ = disp_power->power_drain();

        power_->set_value(
            format_power_fraction(last_power_supplied_, last_power_used_));
    }

    if (power_) {
        power_->update(pfrm, delta);

        if (not persistent_ui_ and last_power_supplied_ >= last_power_used_) {
            power_hide_timer_ += delta;
            if (power_hide_timer_ > seconds(4)) {
                power_.reset();
                power_hide_timer_ = 0;
            }
        }
    } else {
        if (persistent_ui_) {
            power_.emplace(pfrm,
                           OverlayCoord{1, 1},
                           147,
                           format_power_fraction(disp_power->power_supply(),
                                                 disp_power->power_drain()),
                           UIMetric::Align::left,
                           UIMetric::Format::fraction);
        }
    }

    if (disp_power->power_drain() > disp_power->power_supply()) {
        // If the player's island power drain exceeds supply, make the UI
        // sticky, so the player knows why his/her weapons aren't doing
        // anything.
        persist_ui();
    }

    if (last_coins_ not_eq app.coins()) {
        coins_.emplace(pfrm,
                       OverlayCoord{1, 2},
                       146,
                       (int)app.coins(),
                       UIMetric::Align::left);

        coins_->set_value(app.coins());
        last_coins_ = app.coins();
    }

    if (coins_) {
        coins_->update(pfrm, delta);

        if (not persistent_ui_) {
            coin_hide_timer_ += delta;
            if (coin_hide_timer_ > seconds(4)) {
                coins_.reset();
                coin_hide_timer_ = 0;
            }
        }
    } else {
        if (persistent_ui_) {
            coins_.emplace(pfrm,
                           OverlayCoord{1, 2},
                           146,
                           (int)app.coins(),
                           UIMetric::Align::left);
        }
    }

    if (app.opponent_island()) {
        for (auto& projectile : app.player_island().projectiles()) {
            app.opponent_island()->test_collision(pfrm, app, *projectile);
        }

        for (auto& projectile : app.opponent_island()->projectiles()) {
            app.player_island().test_collision(pfrm, app, *projectile);
        }

        for (auto& projectile : app.player_island().projectiles()) {
            app.player_island().test_collision(pfrm, app, *projectile);
        }

        for (auto& projectile : app.opponent_island()->projectiles()) {
            app.opponent_island()->test_collision(pfrm, app, *projectile);
        }
    }

    return ret;
}



void WorldScene::persist_ui()
{
    persistent_ui_ = true;
}



void WorldScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    if (app.macrocosm()) {
        Platform::fatal("entering 2d overworld from macro mode!");
    }

    camera_update_timer_ = milliseconds(1000);

    last_coins_ = app.coins();

    // If we came from another world scene where the coins were visible...
    if (auto last = dynamic_cast<WorldScene*>(&prev)) {
        if (last->coins_) {
            coins_.emplace(pfrm,
                           OverlayCoord{1, 2},
                           146,
                           (int)app.coins(),
                           UIMetric::Align::left);
        }

        Island* disp_power =
            power_fraction_opponent_island_
                ? (app.opponent_island() ? app.opponent_island()
                                         : &app.player_island())
                : &app.player_island();


        if (last->power_) {
            power_.emplace(pfrm,
                           OverlayCoord{1, 1},
                           147,
                           format_power_fraction(disp_power->power_supply(),
                                                 disp_power->power_drain()),
                           UIMetric::Align::left,
                           UIMetric::Format::fraction);
        }

        if (power_fraction_opponent_island_) {
            last_power_supplied_ = disp_power->power_supply();
            last_power_used_ = disp_power->power_drain();
        } else {
            last_power_supplied_ = last->last_power_supplied_;
            last_power_used_ = last->last_power_used_;
        }
    }
}



void WorldScene::exit(Platform& pfrm, App& app, Scene& next)
{
    coins_.reset();
    power_.reset();
}



void WorldScene::far_camera()
{
    far_camera_ = true;
}



void WorldScene::near_camera()
{
    far_camera_ = false;
}



void WorldScene::set_pause_icon(Platform& pfrm, u16 icon)
{
    auto st = calc_screen_tiles(pfrm);

    if (icon == 0) {
        pfrm.set_tile(Layer::overlay, st.x - 3, 1, 0);
        pfrm.set_tile(Layer::overlay, st.x - 2, 1, 0);
        pfrm.set_tile(Layer::overlay, st.x - 3, 2, 0);
        pfrm.set_tile(Layer::overlay, st.x - 2, 2, 0);
    } else {
        auto t = pfrm.get_tile(Layer::overlay, st.x - 2, 1);
        if (t == icon) {
            return;
        }

        t = icon;

        pfrm.set_tile(Layer::overlay, st.x - 3, 1, t++);
        pfrm.set_tile(Layer::overlay, st.x - 2, 1, t++);
        pfrm.set_tile(Layer::overlay, st.x - 3, 2, t++);
        pfrm.set_tile(Layer::overlay, st.x - 2, 2, t);
    }
}



} // namespace skyland
