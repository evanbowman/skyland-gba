////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "worldScene.hpp"
#include "boxedDialogScene.hpp"
#include "easyModeRewindScene.hpp"
#include "globals.hpp"
#include "multiplayerReadyScene.hpp"
#include "multiplayerSettingsScene.hpp"
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
#include "skyland/latency.hpp"
#include "skyland/scene/playerIslandDestroyedScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"
#include "surrenderWaitScene.hpp"



namespace skyland
{



static void apply_gamespeed(Time& delta)
{
    switch (APP.game_speed()) {
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



static void show_multiplayer_pauses_remaining()
{
    auto& g = globals();
    auto st = calc_screen_tiles();

    auto current = PLATFORM.get_tile(Layer::overlay, st.x - 3, 3);
    if (current == 410) {
        // Already assigned, skip integer->text conversion below.
        return;
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 3, 3, 410);
    Text t(OverlayCoord{u8(st.x - 2), 3});
    t.assign("x");
    t.append(g.multiplayer_pauses_remaining_);
    t.__detach();
}



static void hide_multiplayer_pauses_remaining()
{
    auto st = calc_screen_tiles();

    auto current = PLATFORM.get_tile(Layer::overlay, st.x - 3, 3);
    if (current not_eq 410) {
        return;
    }

    PLATFORM.set_tile(Layer::overlay, st.x - 3, 3, 0);
    PLATFORM.set_tile(Layer::overlay, st.x - 2, 3, 0);
    PLATFORM.set_tile(Layer::overlay, st.x - 1, 3, 0);
}



void set_gamespeed(GameSpeed speed)
{
    if (APP.game_speed() == GameSpeed::stopped and
        speed not_eq GameSpeed::stopped) {

        for (auto& room : APP.player_island().rooms()) {
            if (room->should_init_ai_awareness_upon_unpause()) {
                room->init_ai_awareness();
            }
        }
    }

    switch (speed) {
    case GameSpeed::stopped:
    case GameSpeed::normal:
        if (APP.game_speed() not_eq GameSpeed::stopped and
            APP.game_speed() not_eq GameSpeed::normal) {
            PLATFORM.speaker().set_music_speed(
                Platform::Speaker::MusicSpeed::regular);
        }
        break;

    case GameSpeed::fast:
        PLATFORM.speaker().set_music_speed(
            Platform::Speaker::MusicSpeed::doubled);
        break;

    case GameSpeed::rewind:
        PLATFORM.speaker().set_music_speed(
            Platform::Speaker::MusicSpeed::reversed);
        break;

    case GameSpeed::slow:
        PLATFORM.speaker().set_music_speed(
            Platform::Speaker::MusicSpeed::halved);
        break;

    case GameSpeed::count:
        // Erroneous enumeration.
        break;
    }

    if (speed == GameSpeed::stopped) {
        PLATFORM.speaker().stash_sounds();
    } else if (APP.game_speed() == GameSpeed::stopped) {
        PLATFORM.speaker().restore_sounds();
    }

    APP.game_speed() = speed;
}



void WorldScene::set_gamespeed(GameSpeed speed)
{
    skyland::set_gamespeed(speed);

    if (not disable_gamespeed_icon_) {
        if (speed == GameSpeed::normal) {
            set_pause_icon(0);
        } else {
            set_pause_icon(gamespeed_icon(speed));
        }
    }
}



ScenePtr ActiveWorldScene::on_player_island_destroyed()
{
    if ((APP.game_mode() == App::GameMode::adventure or
         APP.game_mode() == App::GameMode::sandbox or
         APP.game_mode() == App::GameMode::skyland_forever) and
        APP.gp_.difficulty_ == GlobalPersistentData::Difficulty::beginner and
        not state_bit_load(StateBit::easy_mode_rewind_declined) and
        APP.persistent_data().lives_ > 0) {
        --APP.persistent_data().lives_;

        return make_scene<EasyModeRewindScene>();
    }

    state_bit_store(StateBit::easy_mode_rewind_declined, false);

    reset_gamespeed();

    APP.time_stream().clear();

    auto& cursor_loc = globals().near_cursor_loc_;

    cursor_loc.x = 0;

    if (APP.game_mode() not_eq App::GameMode::multiplayer and
        APP.game_mode() not_eq App::GameMode::co_op) {
        PLATFORM.sleep(4);
    }

    APP.effects().clear();
    return make_scene<PlayerIslandDestroyedScene>(&APP.player_island());
}



ScenePtr ActiveWorldScene::try_surrender()
{
    auto o = APP.opponent_island();

    if (not APP.player_island().is_boarded() and
        o->offensive_capabilities() == 0 and o->character_count() and
        o->projectiles().empty()) {

        state_bit_store(StateBit::surrender_offered, true);

        // The final boss will never surrender.
        if (APP.world_graph().nodes_[APP.current_world_location()].type_ not_eq
            WorldGraph::Node::Type::corrupted) {

            return make_scene<SurrenderWaitScene>();
        }
    }

    return null_scene();
}



void WorldScene::reset_gamespeed()
{
    set_gamespeed(GameSpeed::normal);
}



SHARED_VARIABLE(energy_glow_color);
SHARED_VARIABLE(spr_energy_color_1);
SHARED_VARIABLE(spr_energy_color_2);



void init_glow_color()
{
    energy_glow_color.set(0x66fff7);
}



void set_glow_color()
{
    auto cl = custom_color(energy_glow_color);

    const auto default_neon_blue_color = 0x66fff7;
    if (energy_glow_color == default_neon_blue_color) {
        cl = APP.environment().shader()(
            ShaderPalette::tile0, custom_color(energy_glow_color), 0, 11);
    }

    PLATFORM_EXTENSION(override_palette, Layer::map_0_ext, 11, cl);
    PLATFORM_EXTENSION(
        override_sprite_palette, 9, custom_color(spr_energy_color_1));
    PLATFORM_EXTENSION(
        override_sprite_palette, 10, custom_color(spr_energy_color_2));
}



ScenePtr ActiveWorldScene::update(Time delta)
{
    APP.player().update(delta);

    APP.opponent().update(delta);

    // set_glow_color();

    if (auto new_scene = WorldScene::update(delta)) {
        return new_scene;
    }


    if (APP.game_mode() not_eq App::GameMode::multiplayer and
        APP.game_mode() not_eq App::GameMode::multiplayer and
        APP.input_setup_info()) {

        bool near = (*APP.input_setup_info())->cons().car()->integer().value_;

        auto next = make_scene<SelInputScene>(
            (*APP.input_setup_info())->cons().cdr(), near);

        APP.input_setup_info().reset();

        return next;
    }


    apply_gamespeed(delta);


    APP.environment().update(delta);


    if (APP.player_island().is_destroyed()) {
        return on_player_island_destroyed();
    }

    if (APP.opponent_island()) {
        if (not state_bit_load(StateBit::surrender_offered) and
            APP.game_mode() == App::GameMode::adventure) {

            if (auto next = try_surrender()) {
                return next;
            }
        }
    }

    if (APP.opponent_island() and APP.opponent_island()->is_destroyed()) {
        reset_gamespeed();

        if (APP.exit_condition() == App::ExitCondition::none) {
            auto& cursor_loc = globals().near_cursor_loc_;

            cursor_loc.x = 0;

            APP.effects().clear();
            return make_scene<PlayerIslandDestroyedScene>(
                APP.opponent_island());
        }
    }

    return null_scene();
}



void WorldScene::display()
{
    APP.environment().display();

    if (APP.game_mode() == App::GameMode::co_op) {
        Sprite cursor;
        cursor.set_size(Sprite::Size::w16_h16);
        cursor.set_texture_index(globals().co_op_cursor_icon_);

        auto cursor_loc = globals().co_op_cursor_;

        if (globals().co_op_cursor_near_) {

            auto origin = APP.player_island().visual_origin();

            int x = cursor_loc.x * 16;
            if (cursor_loc.x == 255) {
                x = -16;
            }

            origin.x += Fixnum::from_integer(x);
            origin.y += Fixnum::from_integer(cursor_loc.y * 16);

            cursor.set_position(origin);
        } else if (APP.opponent_island()) {
            auto origin = APP.opponent_island()->visual_origin();

            int x = cursor_loc.x * 16;
            if (cursor_loc.x == 255) {
                x = -16;
            }

            origin.x += Fixnum::from_integer(x);
            origin.y += Fixnum::from_integer(cursor_loc.y * 16);

            cursor.set_position(origin);
        }

        PLATFORM.screen().draw(cursor);
    }

    APP.player_island().display();

    if (APP.opponent_island()) {
        APP.opponent_island()->display();
    }

    for (auto& effect : APP.effects()) {
        PLATFORM.screen().draw(effect->sprite());
    }

    // NOTE: drawn separately because we want UI effects to appear above fire
    // effects.
    APP.player_island().display_fires();
    if (APP.opponent_island()) {
        APP.opponent_island()->display_fires();
    }

    for (auto& bird : APP.birds()) {
        PLATFORM.screen().draw(bird->sprite());
    }
}



static u32 format_power_fraction(Power avail, Power used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



bool WorldScene::camera_update_check_key()
{
    return APP.player().key_pressed(Key::left) or
           APP.player().key_pressed(Key::right) or
           APP.player().key_pressed(Key::up) or
           APP.player().key_pressed(Key::down) or
           APP.player().key_pressed(Key::select);
}



ScenePtr WorldScene::make_dialog()
{
    if (APP.dialog_buffer()) {
        auto buffer = std::move(*APP.dialog_buffer());
        APP.dialog_buffer().reset();
        return make_scene<BoxedDialogSceneWS>(std::move(buffer));
    }
    return null_scene();
}



void WorldScene::multiplayer_vs_timeout_step(Time delta)
{
    if (MultiplayerSettingsScene::timeout_frequency() == 0) {
        return;
    }

    if (APP.game_speed() == GameSpeed::stopped) {
        auto& tm = globals().multiplayer_timeout_remaining_;
        tm -= delta;
        if (tm <= 0) {
            tm += MultiplayerSettingsScene::timeout_duration();
            set_gamespeed(GameSpeed::normal);
        }

        if (not disable_ui_) {
            if (tm / (1 << 20) < globals().multiplayer_timeout_repaint_) {
                StringBuffer<30> msg = "timeout! ";
                const auto rem = tm / seconds(1);
                msg += stringify(rem);

                const u8 margin = centered_text_margins(msg.length());

                for (u32 x = margin - 2; x < msg.length() + 2; ++x) {
                    PLATFORM.set_tile(Layer::overlay, x, 4, 0);
                }

                globals().multiplayer_timeout_text_.emplace(
                    msg.c_str(), OverlayCoord{margin, 4});
            }
        }
        globals().multiplayer_timeout_repaint_ = tm / (1 << 20);
    } else {
        auto& tm = globals().multiplayer_timeout_countdown_;
        tm -= delta;
        if (tm <= 0) {
            tm += MultiplayerSettingsScene::timeout_frequency();
            set_gamespeed(GameSpeed::stopped);
        }

        if (tm < seconds(10)) {
            if (tm / (1 << 20) < globals().multiplayer_timeout_repaint_) {
                StringBuffer<30> msg = "timeout in ";
                const auto rem = tm / seconds(1);
                msg += stringify(rem);
                msg += "...";

                const u8 margin = centered_text_margins(msg.length());

                for (u32 x = margin - 2; x < msg.length() + 2; ++x) {
                    PLATFORM.set_tile(Layer::overlay, x, 4, 0);
                }

                globals().multiplayer_timeout_text_.emplace(
                    msg.c_str(), OverlayCoord{margin, 4});
            }
        } else {
            globals().multiplayer_timeout_text_.reset();
        }
        globals().multiplayer_timeout_repaint_ = tm / (1 << 20);
    }
}



ScenePtr WorldScene::update(Time delta)
{
    auto& g = globals();

    if (not PLATFORM.network_peer().is_connected()) {
        // We scale game updates based on frame delta. But if the game starts to
        // lag a lot, the logic can start to get screwed up, so at some point,
        // the player will in fact start to notice the lag.
        delta = std::min(delta, seconds(1) / 18);
    }


    Time world_delta = delta;

    if (not PLATFORM.network_peer().is_connected()) {
        // NOTE: we can't clamp the clock delta in multiplayer modes of course!

        // The game uses clock deltas for update logic throughout the code, but
        // huge deltas mess up the collision checking, so in certain cases, it's
        // better to just clamp the delta time and allow some lag.
        //
        // i.e.: lag the game if it lags slower than 30fps.
        world_delta = clamp(world_delta, seconds(0), seconds(1) / 30);
    }

    apply_gamespeed(world_delta);

    APP.delta_fp() = world_delta;

    APP.update_parallax(world_delta);


    APP.stat_timer().count_up(delta);
    APP.level_timer().count_up(world_delta);
    APP.time_stream().update(world_delta);


    auto& mt_prep_timer = g.multiplayer_prep_timer_;

    auto& mt_prep_seconds = g.multiplayer_prep_seconds_;


    if (PLATFORM.network_peer().is_connected()) {
        if (mt_prep_seconds) {
            if (APP.game_speed() not_eq GameSpeed::stopped and
                not disable_ui_) {

                mt_prep_timer += delta;
                if (mt_prep_timer > seconds(1)) {
                    mt_prep_timer -= seconds(1);
                    mt_prep_seconds--;

                    if (mt_prep_seconds == 0 and
                        APP.game_mode() not_eq App::GameMode::co_op) {
                        return make_scene<MultiplayerReadyScene>();
                    }

                    StringBuffer<30> msg = "get ready! 0";
                    msg += stringify(mt_prep_seconds / 60);
                    msg += ":";
                    const auto rem = mt_prep_seconds % 60;
                    if (rem < 10) {
                        msg += "0";
                    }
                    msg += stringify(rem);

                    const u8 margin = centered_text_margins(msg.length());


                    g.multiplayer_prep_text_.emplace(msg.c_str(),
                                                     OverlayCoord{margin, 4});
                }
            }
        } else {
            g.multiplayer_prep_text_.reset();
            if (APP.game_mode() == App::GameMode::multiplayer) {
                multiplayer_vs_timeout_step(delta);
            }
        }
    } else {
        g.multiplayer_prep_text_.reset();
    }


    auto tapped_topright_corner = [&] {
        if (auto pos = APP.player().tap_released()) {
            auto sz = PLATFORM.screen().size();
            if (pos->x > sz.x - 36 and pos->y < 36) {
                return true;
            }
        }
        return false;
    };

    auto ret = null_scene();


    if (APP.game_mode() == App::GameMode::multiplayer) {
        // Pauses unsupported in vs mode...
    } else if (not noreturn_ and
               (APP.player().key_up(Key::alt_1) or tapped_topright_corner())) {
        if (APP.game_speed() not_eq GameSpeed::stopped) {

            bool can_pause = true;

            if (PLATFORM.network_peer().is_connected()) {
                if (not g.multiplayer_pauses_remaining_) {
                    can_pause = false;
                    PLATFORM.speaker().play_sound("beep_error", 3);
                    set_gamespeed(GameSpeed::normal);
                    auto future_scene = []() {
                        return make_scene<ReadyScene>();
                    };
                    auto str = SYSTR(error_no_more_pauses);
                    ret = make_scene<NotificationScene>(str->c_str(),
                                                        future_scene);
                } else {
                    g.multiplayer_pause_owner_ = true;
                    g.multiplayer_pauses_remaining_--;
                }
            }

            if (can_pause) {
                APP.pause_count()++;
                set_gamespeed(GameSpeed::stopped);

                if (PLATFORM.network_peer().is_connected()) {
                    network::packet::Paused pkt;
                    pkt.status_ = true;
                    network::transmit(pkt);
                }
            }

        } else {

            bool can_unpause = true;

            if (PLATFORM.network_peer().is_connected()) {
                if (not g.multiplayer_pause_owner_) {
                    can_unpause = false;
                    PLATFORM.speaker().play_sound("beep_error", 3);
                } else {
                    g.multiplayer_pause_owner_ = false;
                }
            }

            if (can_unpause) {
                if (PLATFORM.network_peer().is_connected()) {
                    network::packet::Paused pkt;
                    pkt.status_ = false;
                    network::transmit(pkt);
                }

                set_gamespeed(GameSpeed::normal);
            }
        }
        APP.player().touch_consume();
    } else if (not noreturn_ and APP.player().key_pressed(Key::alt_1) and
               not PLATFORM.network_peer().is_connected()) {
        set_gamespeed_keyheld_timer_ += delta;
        if (set_gamespeed_keyheld_timer_ > milliseconds(300)) {
            return make_scene<SetGamespeedScene>();
        }
    } else {
        set_gamespeed_keyheld_timer_ = 0;
    }

    if (APP.opponent_island()) {

        const bool show_opponent_interior =
            (APP.player_island().has_radar() and
             not static_cast<Opponent&>(APP.opponent_island()->owner())
                     .is_friendly()) or
            APP.opponent_island()->is_boarded() or
            (APP.player_island().interior_visible() and
             APP.game_mode() == App::GameMode::sandbox);

        if (not APP.opponent_island()->interior_visible() and
            show_opponent_interior) {

            show_island_interior(APP.opponent_island());

        } else if (APP.opponent_island()->interior_visible() and
                   not show_opponent_interior) {

            show_island_exterior(APP.opponent_island());
        }

        auto opp_isle = APP.opponent_island();
        auto opp_drift = opp_isle->get_drift();
        auto opp_pos = opp_isle->get_position();

        auto pl_terrain = (int)APP.player_island().terrain().size();

        // Hey, I threw this code together in a panic for a game jam, I know
        // this is illegible. Drift opponent island toward the player, until
        // a certain distance. If the player extends the terrain on his own
        // island, drift the opponent island away to maintain the ideal
        // distance between the two.
        if ((opp_drift < 0.0_fixed and
             opp_pos.x.as_integer() <= pl_terrain * 16 + 48) or
            (opp_drift > 0.0_fixed and
             opp_pos.x.as_integer() > (int)pl_terrain * 16 + 48)) {

            opp_isle->set_position(
                {Fixnum(pl_terrain * 16 + 48), Fixnum(opp_pos.y)});
            opp_isle->set_drift(0.0_fixed);

            APP.on_timeout(milliseconds(500),
                           []() { invoke_hook("on-converge"); });
        }

        if (opp_drift == 0.0_fixed) {
            if (opp_pos.x.as_integer() <
                (int)APP.player_island().terrain().size() * 16 + 48) {
                opp_isle->set_drift(0.00003_fixed);
            }
        }
    }

    if (camera_update_check_key()) {
        camera_update_timer_ = milliseconds(500);
    }

    if (APP.camera()->is_shaking() or camera_update_timer_ > 0 or
        APP.player().touch_current() or APP.camera()->always_update()) {

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

        if (APP.opponent_island() and UNLIKELY(far_camera_)) {
            auto& cursor_loc = g.far_cursor_loc_;
            APP.camera()->update(
                *APP.opponent_island(), cursor_loc, delta, false);
        } else {
            auto& cursor_loc = g.near_cursor_loc_;
            APP.camera()->update(APP.player_island(), cursor_loc, delta, true);
        }
    }

    if (not noreturn_ and APP.dialog_buffer()) {
        return make_dialog();
    }


    APP.player_island().update(world_delta);

    if (APP.opponent_island()) {
        APP.opponent_island()->update(world_delta);
    }

    update_entities(world_delta, APP.effects());

    update_entities(world_delta, APP.birds());


    if (APP.game_mode() == App::GameMode::co_op) {
        if (APP.game_speed() not_eq GameSpeed::stopped) {
            hide_multiplayer_pauses_remaining();
        } else {
            set_pause_icon(gamespeed_icon(APP.game_speed()));

            if (PLATFORM.network_peer().is_connected() and not disable_ui_) {
                show_multiplayer_pauses_remaining();
            }
        }
    } else {
        if (not disable_gamespeed_icon_) {
            if (APP.game_speed() == GameSpeed::normal) {
                set_pause_icon(0);
            } else {
                set_pause_icon(gamespeed_icon(APP.game_speed()));
            }
        }
    }


    if (not disable_ui_) {
        Island* disp_power =
            power_fraction_opponent_island_
                ? (APP.opponent_island() ? APP.opponent_island()
                                         : &APP.player_island())
                : &APP.player_island();


        if (force_show_power_usage_) {
            force_show_power_usage_ = false;

            power_.emplace(OverlayCoord{1, 1},
                           147,
                           format_power_fraction(disp_power->power_supply(),
                                                 disp_power->power_drain()),
                           UIMetric::Align::left,
                           UIMetric::Format::fraction);
        }

        if (last_power_supplied_ not_eq disp_power->power_supply() or
            last_power_used_ not_eq disp_power->power_drain()) {

            last_power_supplied_ = disp_power->power_supply();
            last_power_used_ = disp_power->power_drain();

            power_->set_value(
                format_power_fraction(last_power_supplied_, last_power_used_));
        }

        if (power_) {
            power_->update(delta);

            if (not persistent_ui_ and
                last_power_supplied_ >= last_power_used_) {
                power_hide_timer_ += delta;
                if (power_hide_timer_ > seconds(4)) {
                    power_.reset();
                    power_hide_timer_ = 0;
                }
            }
        } else {
            if (persistent_ui_) {
                power_.emplace(OverlayCoord{1, 1},
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
            if (not disable_ui_) {
                persist_ui();
            }
        }

        if (last_coins_ not_eq APP.coins()) {
            coins_.emplace(OverlayCoord{1, 2},
                           146,
                           (int)APP.coins(),
                           UIMetric::Align::left);

            coins_->set_value(APP.coins());
            last_coins_ = APP.coins();
        }

        if (coins_) {
            coins_->update(delta);

            if (not persistent_ui_) {
                coin_hide_timer_ += delta;
                if (coin_hide_timer_ > seconds(4)) {
                    coins_.reset();
                    coin_hide_timer_ = 0;
                }
            }
        } else {
            if (persistent_ui_) {
                coins_.emplace(OverlayCoord{1, 2},
                               146,
                               (int)APP.coins(),
                               UIMetric::Align::left);
            }
        }
    }

    if (APP.opponent_island()) {
        for (auto& projectile : APP.player_island().projectiles()) {
            APP.opponent_island()->test_collision(*projectile);
        }

        for (auto& projectile : APP.opponent_island()->projectiles()) {
            APP.player_island().test_collision(*projectile);
        }

        for (auto& projectile : APP.player_island().projectiles()) {
            APP.player_island().test_collision(*projectile);
        }

        for (auto& projectile : APP.opponent_island()->projectiles()) {
            APP.opponent_island()->test_collision(*projectile);
        }
    }

    return ret;
}



void WorldScene::persist_ui()
{
    persistent_ui_ = true;
}



void WorldScene::unpersist_ui()
{
    persistent_ui_ = false;
}



void WorldScene::enter(Scene& prev)
{
    if (APP.macrocosm()) {
        Platform::fatal("entering 2d overworld from macro mode!");
    }

    camera_update_timer_ = milliseconds(1000);

    last_coins_ = APP.coins();

    if (auto last = prev.cast_world_scene()) {

        if (disable_ui_) {
            last->coins_.reset();
            last->power_.reset();
        } else {
            // If we came from another world scene where the coins were visible...
            if (last->coins_) {
                coins_.emplace(OverlayCoord{1, 2},
                               146,
                               (int)APP.coins(),
                               UIMetric::Align::left);
            }

            Island* disp_power =
                power_fraction_opponent_island_
                    ? (APP.opponent_island() ? APP.opponent_island()
                                             : &APP.player_island())
                    : &APP.player_island();


            if (last->power_) {
                power_.emplace(OverlayCoord{1, 1},
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
}



void WorldScene::exit(Scene& next)
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



void WorldScene::set_pause_icon(u16 icon)
{
    auto st = calc_screen_tiles();

    if (icon == 0) {
        auto t = PLATFORM.get_tile(Layer::overlay, st.x - 2, 1);
        if (t) {
            PLATFORM.set_tile(Layer::overlay, st.x - 3, 1, 0);
            PLATFORM.set_tile(Layer::overlay, st.x - 2, 1, 0);
            PLATFORM.set_tile(Layer::overlay, st.x - 3, 2, 0);
            PLATFORM.set_tile(Layer::overlay, st.x - 2, 2, 0);
        }
    } else {
        auto t = PLATFORM.get_tile(Layer::overlay, st.x - 2, 1);
        if (t == icon) {
            return;
        }

        t = icon;

        PLATFORM.set_tile(Layer::overlay, st.x - 3, 1, t++);
        PLATFORM.set_tile(Layer::overlay, st.x - 2, 1, t++);
        PLATFORM.set_tile(Layer::overlay, st.x - 3, 2, t++);
        PLATFORM.set_tile(Layer::overlay, st.x - 2, 2, t);
    }
}



bool WorldScene::hide_chr_icon() const
{
    return false;
}



} // namespace skyland
