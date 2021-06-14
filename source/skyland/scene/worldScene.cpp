#include "worldScene.hpp"
#include "globals.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> WorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.paused()) {
        app.updateParallax(delta);
    }

    if (pfrm.keyboard().down_transition<Key::select>()) {
        app.paused() = not app.paused();
        if (not app.paused()) {
            set_pause_icon(pfrm, false);
        }
    }

    if (app.encountered_island()) {
        // Hey, I threw this code together in a panic for a game jam, I know
        // this is illegible. Drift encountered island toward the player, until
        // a certain distance. If the player extends the terrain on his own
        // island, drift the encountered island away to maintain the ideal
        // distance between the two.
        if ((app.encountered_island()->get_drift() < 0 and
             (int) app.encountered_island()->get_position().x <=
                 (int)app.player_island().terrain().size() * 16 + 48) or
            (app.encountered_island()->get_drift() > 0 and
             (int) app.encountered_island()->get_position().x >
                 (int)app.player_island().terrain().size() * 16 + 48)) {

            app.encountered_island()->set_position(
                {(Float)app.player_island().terrain().size() * 16 + 48,
                 app.encountered_island()->get_position().y});
            app.encountered_island()->set_drift(0);
        }

        if (app.encountered_island()->get_drift() == 0) {
            if ((int)app.encountered_island()->get_position().x <
                (int)app.player_island().terrain().size() * 16 + 48) {
                app.encountered_island()->set_drift(0.00003f);
            }
        }
    }

    if (pfrm.keyboard().any_pressed<
        Key::left, Key::right, Key::up, Key::down, Key::alt_1>()) {
        camera_update_timer_ = milliseconds(500);
    }

    if (app.camera().is_shaking() or
        camera_update_timer_ > 0) {
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

        if (app.encountered_island() and UNLIKELY(far_camera_)) {
            auto& cursor_loc =
                std::get<SkylandGlobalData>(globals()).far_cursor_loc_;
            app.camera().update(
                                pfrm, *app.encountered_island(), cursor_loc, delta, false);
        } else {
            auto& cursor_loc =
                std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
            app.camera().update(pfrm, app.player_island(), cursor_loc, delta, true);
        }

    }


    if (pfrm.keyboard().down_transition<Key::alt_1>()) {
        if (app.player_island().interior_visible()) {
            pfrm.load_tile0_texture("tilesheet");
            app.player_island().render_exterior(pfrm);
        } else {
            pfrm.load_tile0_texture("tilesheet_interior");
            app.player_island().render_interior(pfrm);
        }
    }

    if (not app.paused()) {
        app.player_island().update(pfrm, app, delta);

        if (app.encountered_island()) {
            app.encountered_island()->update(pfrm, app, delta);
        }
    } else {
        set_pause_icon(pfrm, true);
    }

    if (last_coins_ not_eq app.coins()) {
        coins_.emplace(pfrm,
                       OverlayCoord{1, 1},
                       146,
                       (int)app.coins(),
                       UIMetric::Align::left);

        coins_->set_value(app.coins());
        last_coins_ = app.coins();
    }

    if (coins_) {
        coins_->update(pfrm, delta);

        if (not persistent_coins_) {
            coin_hide_timer_ += delta;
            if (coin_hide_timer_ > seconds(2)) {
                coins_.reset();
                coin_hide_timer_ = 0;
            }
        }
    } else {
        if (persistent_coins_) {
            coins_.emplace(pfrm,
                           OverlayCoord{1, 1},
                           146,
                           (int)app.coins(),
                           UIMetric::Align::left);
        }
    }

    return null_scene();
}



void WorldScene::persist_coins()
{
    persistent_coins_ = true;
}



void WorldScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    camera_update_timer_ = milliseconds(500);

    last_coins_ = app.coins();

    // If we came from another world scene where the coins were visible...
    if (dynamic_cast<WorldScene*>(&prev)->coins_) {
        coins_.emplace(pfrm,
                       OverlayCoord{1, 1},
                       146,
                       (int)app.coins(),
                       UIMetric::Align::left);
    }
}



void WorldScene::exit(Platform& pfrm, App& app, Scene& next)
{
    coins_.reset();
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
