#include "worldScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "globals.hpp"



namespace skyland {



ScenePtr<Scene> WorldScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (not app.paused()) {
        app.updateParallax(delta);
    }

    if (pfrm.keyboard().down_transition<Key::select>()) {
        app.paused() = not app.paused();
    }

    auto& cursor_loc = std::get<SkylandGlobalData>(globals()).near_cursor_loc_;
    app.camera().update(pfrm, app.player_island(), cursor_loc, delta);

    if (not app.paused()) {
        pfrm.set_scroll(app.player_island().layer(),
                        -app.player_island().get_position().cast<u16>().x,
                        -app.player_island().get_position().cast<u16>().y -
                        app.player_island().get_ambient_movement());
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

            pfrm.set_scroll(app.encountered_island()->layer(),
                        -app.encountered_island()->get_position().cast<u16>().x,
                        -app.encountered_island()->get_position().cast<u16>().y -
                        app.encountered_island()->get_ambient_movement());

        }
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



} // namespace skyland
