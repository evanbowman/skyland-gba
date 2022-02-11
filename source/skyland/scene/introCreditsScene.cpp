#include "introCreditsScene.hpp"
#include "newgameScene.hpp"
#include "number/random.hpp"
#include "skyland/scene_pool.hpp"
#include "titleScreenScene.hpp"



namespace skyland {



void IntroCreditsScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.load_overlay_texture("overlay");

    pfrm.screen().fade(1.f);

    rng::critical_state = 2021;
}



void IntroCreditsScene::exit(Platform& pfrm, App&, Scene& next)
{
    text_.reset();
}



ScenePtr<Scene>
IntroCreditsScene::update(Platform& pfrm, App&, Microseconds delta)
{
    timer_ += delta;

    if (wait_) {
        if (timer_ > milliseconds(500)) {
            wait_ = false;
            timer_ = 0;

            const char* text = "Evan Bowman presents";

            const auto st = calc_screen_tiles(pfrm);
            u8 margin = centered_text_margins(pfrm, utf8::len(text));

            text_.emplace(pfrm, text, OverlayCoord{margin,
                                                   (u8)(st.y / 2 - 3)});

            if (utf8::len(text) % 2 not_eq 0) {
                pfrm.set_overlay_origin(-4, 0);
            }
        }
    } else if (text_) {
        if (timer_ > seconds(4) or key_down<Key::action_2>(pfrm)) {
            text_.reset();
            timer_ = 0;
        }
    } else {
        pfrm.set_overlay_origin(0, 0);
        // if (timer_ > milliseconds(600)) {
        info(pfrm, "going to title screen");
        return scene_pool::alloc<TitleScreenScene>();
        // }
    }

    return null_scene();
}



} // namespace skyland
