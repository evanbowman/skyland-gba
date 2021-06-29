#include "worldScene.hpp"
#include "boxedDialogScene.hpp"
#include "globals.hpp"
#include "number/random.hpp"
#include "platform/platform.hpp"
#include "scriptHookScene.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/birbs/smolBirb.hpp"
#include "skyland/scene/playerIslandDestroyedScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene>
ActiveWorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (auto new_scene = WorldScene::update(pfrm, app, delta)) {
        return new_scene;
    }


    if (app.player_island().is_destroyed()) {
        // app.on_timeout(pfrm, milliseconds(120), [](Platform& pfrm, App& app) {
        //     auto origin = app.player_island().origin();
        //     origin.x += (app.player_island().terrain().size() / 2) * 16;
        //     medium_explosion(pfrm, app, origin);
        // });
        auto& cursor_loc =
            std::get<SkylandGlobalData>(globals()).near_cursor_loc_;

        cursor_loc.x = 0;

        app.effects().clear();
        return scene_pool::alloc<PlayerIslandDestroyedScene>(
            &app.player_island());
    }


    if (app.opponent_island() and app.opponent_island()->is_destroyed()) {

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
    if (not app.paused()) {
        app.update_parallax(delta);
    }

    if (pfrm.network_peer().is_connected()) {
        // We don't allow pausing during multiplayer games yet. Makes things
        // simpler.
        app.paused() = false;
    } else if (pfrm.keyboard().down_transition<Key::alt_1>()) {
        app.paused() = not app.paused();
        if (not app.paused()) {
            set_pause_icon(pfrm, false);
        }
    }

    if (app.opponent_island()) {

        const bool show_opponent_interior = app.player_island().has_radar() or
                                            app.opponent_island()->is_boarded();

        if (not app.opponent_island()->interior_visible() and
            show_opponent_interior) {

            pfrm.load_tile1_texture("tilesheet_enemy_0_interior");

            app.opponent_island()->render_interior(pfrm);
        } else if (app.opponent_island()->interior_visible() and
                   not show_opponent_interior) {

            pfrm.load_tile1_texture("tilesheet_enemy_0");

            app.opponent_island()->render_exterior(pfrm);
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
            app.opponent_island()->set_drift(0);

            app.on_timeout(pfrm, milliseconds(500), [](Platform& pfrm, App&) {
                invoke_hook(pfrm, "after-converge-hook");
            });
        }

        if (app.opponent_island()->get_drift() == 0) {
            if ((int)app.opponent_island()->get_position().x <
                (int)app.player_island().terrain().size() * 16 + 48) {
                app.opponent_island()->set_drift(0.00003f);
            }
        }
    }

    if (pfrm.keyboard()
            .any_pressed<Key::left,
                         Key::right,
                         Key::up,
                         Key::down,
                         Key::select>()) {
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


    if (pfrm.keyboard().down_transition<Key::select>()) {
        if (app.player_island().interior_visible()) {
            pfrm.load_tile0_texture("tilesheet");
            app.player_island().render_exterior(pfrm);
        } else {
            pfrm.load_tile0_texture("tilesheet_interior");
            app.player_island().render_interior(pfrm);
        }
    }

    if (not app.paused()) {

        if (app.dialog_buffer()) {
            auto buffer = std::move(*app.dialog_buffer());
            app.dialog_buffer().reset();
            const bool answer = app.dialog_expects_answer();
            app.dialog_expects_answer() = false;
            return scene_pool::alloc<BoxedDialogScene>(std::move(buffer),
                                                       answer);
        }

        app.player_island().update(pfrm, app, delta);

        if (app.opponent_island()) {
            app.opponent_island()->update(pfrm, app, delta);
        }

        update_entities(pfrm, app, delta, app.effects());
        for (auto& effect : app.effects()) {
            effect->update(pfrm, app, delta);
        }

    } else {
        set_pause_icon(pfrm, true);
    }


    if (last_power_supplied_ not_eq app.player_island().power_supply() or
        last_power_used_ not_eq app.player_island().power_drain()) {

        last_power_supplied_ = app.player_island().power_supply();
        last_power_used_ = app.player_island().power_drain();

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
            power_.emplace(
                pfrm,
                OverlayCoord{1, 1},
                147,
                format_power_fraction(app.player_island().power_supply(),
                                      app.player_island().power_drain()),
                UIMetric::Align::left,
                UIMetric::Format::fraction);
        }
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

    if (not app.paused()) {
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
    }

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

        if (last->power_) {
            power_.emplace(
                pfrm,
                OverlayCoord{1, 1},
                147,
                format_power_fraction(app.player_island().power_supply(),
                                      app.player_island().power_drain()),
                UIMetric::Align::left,
                UIMetric::Format::fraction);
        }

        last_power_supplied_ = last->last_power_supplied_;
        last_power_used_ = last->last_power_used_;
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



void WorldScene::set_pause_icon(Platform& pfrm, bool paused)
{
    auto st = calc_screen_tiles(pfrm);

    if (not paused) {
        pfrm.set_tile(Layer::overlay, st.x - 3, 1, 0);
        pfrm.set_tile(Layer::overlay, st.x - 2, 1, 0);
        pfrm.set_tile(Layer::overlay, st.x - 3, 2, 0);
        pfrm.set_tile(Layer::overlay, st.x - 2, 2, 0);
    } else {
        auto t = pfrm.get_tile(Layer::overlay, st.x - 2, 1);
        if (t == 177) {
            return;
        }

        t = 177;

        pfrm.set_tile(Layer::overlay, st.x - 3, 1, t++);
        pfrm.set_tile(Layer::overlay, st.x - 2, 1, t++);
        pfrm.set_tile(Layer::overlay, st.x - 3, 2, t++);
        pfrm.set_tile(Layer::overlay, st.x - 2, 2, t);
    }
}



} // namespace skyland
