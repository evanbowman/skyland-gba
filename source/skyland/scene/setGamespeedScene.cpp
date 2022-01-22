#include "setGamespeedScene.hpp"
#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"



namespace skyland {



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
                return scene_pool::alloc<RewindScene>();
            } else {
                set_gamespeed(pfrm, app, GameSpeed::rewind);
                return scene_pool::alloc<RewindScene>();
            }
        } else {
            set_gamespeed(pfrm, app, (GameSpeed)selection_);
            return scene_pool::alloc<ReadyScene>();
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
}



void SetGamespeedScene::exit(Platform& pfrm, App& app, Scene&)
{
    pfrm.load_overlay_texture("overlay");
    pfrm.fill_overlay(0);
    set_gamespeed(pfrm, app, (GameSpeed)selection_);
}



} // namespace skyland
