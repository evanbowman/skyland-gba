#include "hintScene.hpp"
#include "platform/platform.hpp"
#include "worldMapScene.hpp"
#include "skyland/scene_pool.hpp"



namespace skyland {


struct HintInfo {
    const char* img_name_;
    const char* text_;
};



std::array<HintInfo, 4> hints = {{
    {"hint_infirmary", "Build an infirmary to heal your crew!"},
    {"hint_goblin", "Use bulkhead doors to protect your power-cores against goblins!"},
    {"hint_plunder", "Raid and plunder castles for extra coins!"},
    {"hint_damaged_core", "If you lose a power-core, systems may shut down til you rebalance power."}
}};



static void show_hint(Platform& pfrm,
                      const HintInfo& info,
                      TextView& text)
{
    pfrm.load_overlay_texture(info.img_name_);

    text.assign(info.text_, {2, 6}, {16, 12}, 0);
}



void HintScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.fill_overlay(82);
    draw_image(pfrm, 82, 18, 4, 12, 10, Layer::overlay);

    show_hint(pfrm, hints[hint_index_], *body_);

    heading_.emplace(pfrm, OverlayCoord{1, 1});
    heading_->assign("tips and tricks",
                     Text::OptColors{{custom_color(0x163061),
                                      custom_color(0xffffff)}});
}


void HintScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
    heading_.reset();
    body_.reset();
    pfrm.fill_overlay(82);

}


ScenePtr<Scene> HintScene::update(Platform& pfrm, App&, Microseconds delta)
{
    switch (state_) {
    case State::scene_intro: {
        static const auto fade_duration = milliseconds(400);
        timer_ += delta;
        if (timer_ < fade_duration) {
            pfrm.screen().fade(1.f - smoothstep(0.f, fade_duration, timer_),
                               ColorConstant::rich_black,
                               {},
                               true,
                               true);
        } else {
            pfrm.screen().fade(0.f);
            timer_ = 0;
            state_ = State::idle;
        }
        break;
    }

    case State::idle:
        if (key_down<Key::action_1>(pfrm)) {
            // if (hint_index_ >= (sizeof(hints) / sizeof(HintInfo))) {
            //     hint_index_ = 0;
            // } else {
            // }
            ++hint_index_;
            hint_index_ %= hints.size();
            heading_.reset();
            timer_ = 0;

            state_ = State::fade_out;
        } else if (key_down<Key::action_2>(pfrm)) {
            return scene_pool::alloc<WorldMapScene>();
        }
        break;

    case State::fade_in: {
        static const auto fade_duration = milliseconds(200);
        timer_ += delta;
        if (timer_ < fade_duration) {
            pfrm.screen().fade(1.f - smoothstep(0.f, fade_duration, timer_),
                               custom_color(0x163061),
                               {},
                               true,
                               true);
        } else {
            pfrm.screen().fade(0.f);
            timer_ = 0;
            state_ = State::idle;
        }
        break;
    }

    case State::swap_img:
        body_.reset();
        pfrm.fill_overlay(82);

        body_.emplace(pfrm);
        draw_image(pfrm, 82, 18, 4, 12, 10, Layer::overlay);
        show_hint(pfrm, hints[hint_index_], *body_);
        state_ = State::fade_in;

        heading_.emplace(pfrm, OverlayCoord{1, 1});
        heading_->assign("tips and tricks",
                         Text::OptColors{{custom_color(0x163061),
                                     custom_color(0xffffff)}});
        break;

    case State::fade_out:
        static const auto fade_duration = milliseconds(200);
        timer_ += delta;
        if (timer_ < fade_duration) {
            pfrm.screen().fade(smoothstep(0.f, fade_duration, timer_),
                               custom_color(0x163061),
                               {},
                               true,
                               true);
        } else {
            pfrm.screen().fade(1.f,
                               custom_color(0x163061),
                               {},
                               true,
                               true);

            timer_ = 0;
            state_ = State::swap_img;
        }
        break;
    }

    return null_scene();
}



}
