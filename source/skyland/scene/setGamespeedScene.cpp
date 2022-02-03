#include "setGamespeedScene.hpp"
#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "swapOverlayTextureScene.hpp"



namespace skyland {



static const char* gamespeed_text(GameSpeed speed)
{
    switch (speed) {
    case GameSpeed::stopped:
        return "paused";
    case GameSpeed::slow:
        return "slow";
    case GameSpeed::normal:
        return "regular";
    case GameSpeed::fast:
        return "fast";
    case GameSpeed::rewind:
        return "rewind";
    case GameSpeed::count:
        return "ERROR";
    }
    return "ERROR";
}



ScenePtr<Scene>
SetGamespeedScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    if (app.player().key_down(pfrm, Key::right)) {
        selection_++;
        selection_ %= (int)GameSpeed::count;
        repaint_selector(pfrm);
    } else if (app.player().key_down(pfrm, Key::left)) {
        selection_--;
        if (selection_ < 0) {
            selection_ = (int)GameSpeed::count - 1;
        }
        repaint_selector(pfrm);
    }

    if (app.player().key_up(pfrm, Key::alt_1)) {
        if ((GameSpeed)selection_ == GameSpeed::rewind) {
            if (app.time_stream().pushes_enabled()) {
                set_gamespeed(pfrm, app, GameSpeed::stopped);

                return scene_pool::alloc<SwapOverlayTextureScene>(
                    "overlay", scene_pool::make_deferred_scene<RewindScene>());
            } else {
                set_gamespeed(pfrm, app, GameSpeed::stopped);
                selection_ = (int)GameSpeed::stopped;
                repaint_selector(pfrm);

                return scene_pool::alloc<SwapOverlayTextureScene>(
                    "overlay", [] {
                        auto future_scene = []() {
                            return scene_pool::alloc<ReadyScene>();
                        };
                        const char* msg = "rewind disabled!";
                        return scene_pool::alloc<NotificationScene>(
                            msg, future_scene);
                    });
            }
        } else {
            set_gamespeed(pfrm, app, (GameSpeed)selection_);

            return scene_pool::alloc<SwapOverlayTextureScene>(
                "overlay", scene_pool::make_deferred_scene<ReadyScene>());
        }
    }

    return null_scene();
}



u16 gamespeed_icon(GameSpeed speed);



void SetGamespeedScene::enter(Platform& pfrm, App& app, Scene&)
{
    selection_ = (int)app.game_speed();

    pfrm.fill_overlay(0);
    pfrm.load_overlay_texture("overlay_gamespeed");

    repaint_selector(pfrm);
}



void SetGamespeedScene::repaint_selector(Platform& pfrm)
{
    auto st = calc_screen_tiles(pfrm);
    for (int i = 0; i < (int)GameSpeed::count; ++i) {
        auto gs = (GameSpeed)((i + selection_) % (int)GameSpeed::count);
        auto t = gamespeed_icon(gs);

        auto start = st.x - 3;

        if (i > 0) {
            start -= 1;
        }

        pfrm.set_tile(Layer::overlay, start - i * 2, 1, t++);
        pfrm.set_tile(Layer::overlay, start - i * 2 + 1, 1, t++);
        pfrm.set_tile(Layer::overlay, start - i * 2, 2, t++);
        pfrm.set_tile(Layer::overlay, start - i * 2 + 1, 2, t);

        pfrm.set_tile(Layer::overlay, start - i * 2, 3, 119);
        pfrm.set_tile(Layer::overlay, start - i * 2 + 1, 3, 119);
    }


    pfrm.set_tile(
        Layer::overlay, (st.x - 5) - 2 * ((int)GameSpeed::count - 1), 1, 424);


    pfrm.set_tile(
        Layer::overlay, (st.x - 5) - 2 * ((int)GameSpeed::count - 1), 2, 128);


    pfrm.set_tile(Layer::overlay, (st.x - 1), 1, 423);


    pfrm.set_tile(Layer::overlay, (st.x - 1), 2, 433);

    // divider
    pfrm.set_tile(Layer::overlay, (st.x - 4), 1, 379);


    pfrm.set_tile(Layer::overlay, (st.x - 4), 2, 379);

    pfrm.set_tile(Layer::overlay, (st.x - 4), 3, 119);

    if (not speed_text_) {
        speed_text_.emplace(pfrm,
                            OverlayCoord{0, u8(calc_screen_tiles(pfrm).y - 1)});
    }
    StringBuffer<30> temp("speed: ");
    temp += gamespeed_text((GameSpeed)selection_);
    speed_text_->assign(temp.c_str());

    const u8 y = calc_screen_tiles(pfrm).y - 2;

    for (int i = 0; i < 30; ++i) {
        pfrm.set_tile(Layer::overlay, i, y, 0);
    }

    for (int i = 0; i < speed_text_->len(); ++i) {
        pfrm.set_tile(Layer::overlay, i, y, 425);
    }
}



void SetGamespeedScene::exit(Platform& pfrm, App& app, Scene&)
{
    speed_text_.reset();
    pfrm.fill_overlay(0);
}



} // namespace skyland
