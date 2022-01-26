#include "hintScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland {


struct HintInfo {
    const char* img_name_;
    const char* text_;
};



std::array<HintInfo, 8> hints = {
    {{"<none>",
      "Press and hold the pause button to set the game speed. Slow motion and "
      "fastforward speeds available! You can even rewind!"},
     {"hint_infirmary", "Build an infirmary to heal your crew!"},
     {"<none>",
      "In the construction menu, the up/down buttons scroll through "
      "categories: defenses, weapons, workshops, power, misc."},
     {"hint_goblin",
      "Use bulkhead doors to protect your power-cores against goblins!"},
     {"<none>",
      "The Glossary, found on the title screen extras menu, describes all "
      "structures in the game. Pressing start with the construction menu open "
      "also opens the Glossary."},
     {"hint_plunder", "Raid and plunder castles for extra coins!"},
     {"hint_damaged_core",
      "If you lose a power-core, systems may shut down til you rebalance "
      "power."},
     {"<none>",
      "For more help, scroll right on the title screen and open the tutorial "
      "viewer!"}}};



static void show_hint(Platform& pfrm, const HintInfo& info, TextView& text)
{
    if (str_cmp("<none>", info.img_name_) == 0) {
        pfrm.load_overlay_texture("hint_infirmary");
        text.assign(info.text_, {2, 4}, {26, 18}, 0);
    } else {
        pfrm.load_overlay_texture(info.img_name_);
        draw_image(pfrm, 82, 18, 4, 12, 10, Layer::overlay);
        text.assign(info.text_, {2, 6}, {16, 12}, 0);
    }
}



void HintScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.fill_overlay(82);

    show_hint(pfrm, hints[hint_index_], *body_);

    heading_.emplace(pfrm, OverlayCoord{1, 1});
    heading_->assign(
        "tips and tricks",
        Text::OptColors{{custom_color(0x163061), custom_color(0xffffff)}});
}


void HintScene::exit(Platform& pfrm, App&, Scene& next)
{
    pfrm.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
    heading_.reset();
    body_.reset();
    pfrm.fill_overlay(0);
}


ScenePtr<Scene> HintScene::update(Platform& pfrm, App& app, Microseconds delta)
{
    switch (state_) {
    case State::scene_intro: {
        static const auto fade_duration = milliseconds(400);
        timer_ += delta;
        if (timer_ < fade_duration) {
            pfrm.screen().schedule_fade(1.f - smoothstep(0.f, fade_duration, timer_),
                                        ColorConstant::rich_black,
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
        if (app.player().key_down(pfrm, Key::action_1)) {
            // if (hint_index_ >= (sizeof(hints) / sizeof(HintInfo))) {
            //     hint_index_ = 0;
            // } else {
            // }
            ++hint_index_;
            hint_index_ %= hints.size();
            heading_.reset();
            timer_ = 0;

            state_ = State::fade_out;
        } else if (app.player().key_down(pfrm, Key::action_2)) {
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
        heading_->assign(
            "tips and tricks",
            Text::OptColors{{custom_color(0x163061), custom_color(0xffffff)}});
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
            pfrm.screen().fade(1.f, custom_color(0x163061), {}, true, true);

            timer_ = 0;
            state_ = State::swap_img;
        }
        break;
    }

    return null_scene();
}



} // namespace skyland
