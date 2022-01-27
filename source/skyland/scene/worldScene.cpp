#include "worldScene.hpp"
#include "boxedDialogScene.hpp"
#include "globals.hpp"
#include "multiplayerReadyScene.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "scriptHookScene.hpp"
#include "selInputScene.hpp"
#include "setGamespeedScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birbs/smolBirb.hpp"
#include "skyland/scene/playerIslandDestroyedScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



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

    // NOTE: we shouldn't even be in this scene if we're rewinding.
    case GameSpeed::rewind:
        delta = 0;
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



void show_island_interior(Platform& pfrm, App& app, Island* island)
{
    if (island == &app.player_island()) {
        pfrm.load_tile0_texture("tilesheet_interior");

    } else {
        pfrm.load_tile1_texture("tilesheet_enemy_0_interior");
    }

    island->render_interior(pfrm, app);

    if (island == &app.player_island()) {
        vram_write_flag(pfrm, app.gp_.flag_img_);
    }
}



void WorldScene::set_gamespeed(Platform& pfrm, App& app, GameSpeed speed)
{
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


    if (app.game_mode() not_eq App::GameMode::multiplayer and
        app.input_setup_info()) {

        auto next = scene_pool::alloc<SelInputScene>(*app.input_setup_info(),
                                                     not is_far_camera());

        app.input_setup_info().reset();

        return next;
    }


    apply_gamespeed(app, delta);

    if (app.player_island().is_destroyed()) {
        reset_gamespeed(pfrm, app);

        app.time_stream().clear();

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        cursor_loc.x = 0;

        if (app.game_mode() not_eq App::GameMode::multiplayer) {
            pfrm.sleep(4);
        }

        app.effects().clear();
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            &app.player_island());
    }


    if (app.opponent_island() and app.opponent_island()->is_destroyed()) {
        reset_gamespeed(pfrm, app);

        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        cursor_loc.x = 0;

        app.effects().clear();
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            &*app.opponent_island());
    }

    return null_scene();
}



void WorldScene::display(Platform& pfrm, App& app)
{
    app.player_island().display(pfrm);

    if (app.opponent_island()) {
        app.opponent_island()->display(pfrm);
    }

    for (auto& effect : app.effects()) {
        pfrm.screen().draw(effect->sprite());
    }
}



static u32 format_power_fraction(Power avail, Power used)
{
    return (avail & 0x0000ffff) | ((used & 0x0000ffff) << 16);
}



ScenePtr<Scene> WorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not pfrm.network_peer().is_connected()) {
        // We scale game updates based on frame delta. But if the game starts to
        // lag a lot, the logic can start to get screwed up, so at some point,
        // the player will in fact start to notice the lag.
        delta = std::min(delta, seconds(1) / 18);
    }


    Microseconds world_delta = delta;
    apply_gamespeed(app, world_delta);

    app.update_parallax(world_delta);


    app.level_timer().count_up(world_delta);
    app.time_stream().update(world_delta);


    auto& mt_prep_timer =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_timer_;

    auto& mt_prep_seconds =
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_seconds_;


    if (pfrm.network_peer().is_connected()) {
        if (mt_prep_seconds) {
            mt_prep_timer += delta;
            if (mt_prep_timer > seconds(1)) {
                mt_prep_timer -= seconds(1);
                mt_prep_seconds--;

                if (mt_prep_seconds == 0) {
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

                std::get<SkylandGlobalData>(globals())
                    .multiplayer_prep_text_.emplace(
                        pfrm, msg.c_str(), OverlayCoord{margin, 4});
            }
        } else {
            std::get<SkylandGlobalData>(globals())
                .multiplayer_prep_text_.reset();
        }
    } else {
        std::get<SkylandGlobalData>(globals()).multiplayer_prep_text_.reset();
    }


    if (pfrm.network_peer().is_connected()) {
        // We don't allow pausing during multiplayer games yet. Makes things
        // simpler.
        set_gamespeed(pfrm, app, GameSpeed::normal);
    } else if (app.player().key_up(pfrm, Key::alt_1)) {
        if (app.game_speed() not_eq GameSpeed::stopped) {
            app.pause_count()++;
            set_gamespeed(pfrm, app, GameSpeed::stopped);
        } else {
            set_gamespeed(pfrm, app, GameSpeed::normal);
        }
    } else if (app.player().key_pressed(pfrm, Key::alt_1)) {
        set_gamespeed_keyheld_timer_ += delta;
        if (set_gamespeed_keyheld_timer_ > milliseconds(300)) {
            return scene_pool::alloc<SetGamespeedScene>();
        }
    } else {
        set_gamespeed_keyheld_timer_ = 0;
    }

    if (app.opponent_island()) {

        const bool show_opponent_interior =
            app.player_island().has_radar() or
            app.opponent_island()->is_boarded() or
            (app.player_island().interior_visible() and
             app.game_mode() == App::GameMode::sandbox);

        if (not app.opponent_island()->interior_visible() and
            show_opponent_interior) {

            show_island_interior(pfrm, app, &*app.opponent_island());

        } else if (app.opponent_island()->interior_visible() and
                   not show_opponent_interior) {

            pfrm.load_tile1_texture("tilesheet_enemy_0");

            app.opponent_island()->render_exterior(pfrm, app);
        }

        // Hey, I threw this code together in a panic for a game jam, I know
        // this is illegible. Drift opponent island toward the player, until
        // a certain distance. If the player extends the terrain on his own
        // island, drift the opponent island away to maintain the ideal
        // distance between the two.
        if ((app.opponent_island()->get_drift() < 0 and
             (int) app.opponent_island()->get_position().x <=
                 (int)app.player_island().terrain().size() * 16 + 48) or
            (app.opponent_island()->get_drift() > 0 and
             (int) app.opponent_island()->get_position().x >
                 (int)app.player_island().terrain().size() * 16 + 48)) {

            app.opponent_island()->set_position(
                {(Float)app.player_island().terrain().size() * 16 + 48,
                 app.opponent_island()->get_position().y});
            app.opponent_island()->set_drift(pfrm, app, 0);

            app.on_timeout(
                pfrm, milliseconds(500), [](Platform& pfrm, App& app) {
                    invoke_hook(pfrm, app, "after-converge-hook");
                });
        }

        if (app.opponent_island()->get_drift() == 0) {
            if ((int)app.opponent_island()->get_position().x <
                (int)app.player_island().terrain().size() * 16 + 48) {
                app.opponent_island()->set_drift(pfrm, app, 0.00003f);
            }
        }
    }

    if (app.player().key_pressed(pfrm, Key::left) or
        app.player().key_pressed(pfrm, Key::right) or
        app.player().key_pressed(pfrm, Key::up) or
        app.player().key_pressed(pfrm, Key::down) or
        app.player().key_pressed(pfrm, Key::select)) {

        camera_update_timer_ = milliseconds(500);
    }

    if (app.camera().is_shaking() or camera_update_timer_ > 0) {
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
            auto& cursor_loc =
                std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
            app.camera().update(
                pfrm, *app.opponent_island(), cursor_loc, delta, false);
        } else {
            auto& cursor_loc =
                std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
            app.camera().update(
                pfrm, app.player_island(), cursor_loc, delta, true);
        }
    }


    if (app.player().key_down(pfrm, Key::select)) {
        if (app.player_island().interior_visible()) {
            pfrm.load_tile0_texture("tilesheet");
            app.player_island().render_exterior(pfrm, app);
            vram_write_flag(pfrm, app.gp_.flag_img_);
        } else {
            show_island_interior(pfrm, app, &app.player_island());
        }
    }

    if (app.dialog_buffer()) {
        auto buffer = std::move(*app.dialog_buffer());
        app.dialog_buffer().reset();
        const bool answer = app.dialog_expects_answer();
        app.dialog_expects_answer() = false;
        return scene_pool::alloc<BoxedDialogScene>(std::move(buffer), answer);
    }


    app.player_island().update(pfrm, app, world_delta);

    if (app.opponent_island()) {
        app.opponent_island()->update(pfrm, app, world_delta);
    }

    update_entities(pfrm, app, world_delta, app.effects());
    for (auto& effect : app.effects()) {
        effect->update(pfrm, app, world_delta);
    }


    if (app.game_speed() not_eq GameSpeed::stopped) {

    } else {
        set_pause_icon(pfrm, gamespeed_icon(app.game_speed()));
    }


    Island* disp_power = power_fraction_opponent_island_
                             ? (app.opponent_island() ? &*app.opponent_island()
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

    // if (app.game_speed() == App::GameSpeed::normal) {
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
    // }

    return null_scene();
}



void WorldScene::persist_ui()
{
    persistent_ui_ = true;
}



void WorldScene::enter(Platform& pfrm, App& app, Scene& prev)
{
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
                ? (app.opponent_island() ? &*app.opponent_island()
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
