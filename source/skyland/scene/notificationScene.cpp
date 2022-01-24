#include "notificationScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



ScenePtr<Scene> NotificationScene::update(Platform& pfrm,
                                          App& app,
                                          Microseconds delta)
{
    if (auto next = ActiveWorldScene::update(pfrm, app, delta)) {
        return next;
    }

    if (app.player().key_down(pfrm, Key::action_1) or
        app.player().key_down(pfrm, Key::action_2) or
        app.player().key_down(pfrm, Key::left) or
        app.player().key_down(pfrm, Key::right) or
        app.player().key_down(pfrm, Key::up) or
        app.player().key_down(pfrm, Key::down)) {

        return next_scene_();
    }

    return null_scene();
}



void NotificationScene::enter(Platform& pfrm, App& app, Scene& prev)
{
    ActiveWorldScene::enter(pfrm, app, prev);

    description_.emplace(pfrm, OverlayCoord{
            0,
            u8(calc_screen_tiles(pfrm).y - 1)
        });

    description_->assign(msg_.c_str());

    for (int i = 0; i < description_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, 18, 425);
    }
}



void NotificationScene::exit(Platform& pfrm, App& app, Scene& next)
{
    ActiveWorldScene::exit(pfrm, app, next);

    description_.reset();
    pfrm.fill_overlay(0);
}



}
