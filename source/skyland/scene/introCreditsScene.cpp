#include "introCreditsScene.hpp"
#include "newgameScene.hpp"
#include "number/random.hpp"
#include "skyland/scene_pool.hpp"
#include "titleScreenScene.hpp"



namespace skyland {



void IntroCreditsScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.load_overlay_texture("overlay_skyland_title");

    pfrm.screen().fade(1.f);

    rng::critical_state = 2021;
}



void IntroCreditsScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.load_overlay_texture("overlay");

    pfrm.set_overlay_origin(0, 0);

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

            text_.emplace(pfrm, text, OverlayCoord{margin, (u8)(st.y / 2 - 3)});
        }
    } else if (text_) {
        if (timer_ > milliseconds(500) and timer_ < milliseconds(2000)) {
            auto amount =
                smoothstep(milliseconds(500), milliseconds(2000), timer_);
            pfrm.set_overlay_origin(0, amount * 30);
        }
        if (timer_ > milliseconds(2000)) {
            pfrm.set_overlay_origin(0, 30);

            draw_image(pfrm, 82, 4, 9, 22, 10, Layer::overlay);
        }

        if (timer_ > milliseconds(3000) and not copyright_text_) {
            const char* copyright_str = "Copyright ©2022";
            copyright_text_.emplace(
                pfrm,
                OverlayCoord{
                    (u8)centered_text_margins(pfrm, utf8::len(copyright_str)),
                    20});

            copyright_text_->assign(copyright_str,
                                    FontColors{ColorConstant::med_blue_gray,
                                               ColorConstant::rich_black});
        }

        auto t = pfrm.screen().touch();
        if (timer_ > milliseconds(5500) or key_down<Key::action_2>(pfrm) or
            t->up_transition()) {
            text_.reset();
            copyright_text_.reset();
            pfrm.fill_overlay(0);
            timer_ = 0;
        }
    } else {
        if (timer_ > milliseconds(400)) {
            pfrm.set_overlay_origin(0, 0);
            info(pfrm, "going to title screen");
            return scene_pool::alloc<TitleScreenScene>();
        }
    }

    return null_scene();
}



} // namespace skyland
