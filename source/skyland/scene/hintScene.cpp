#include "hintScene.hpp"
#include "platform/platform.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"
#include "skyland/systemString.hpp"



namespace skyland {


struct HintInfo
{
    const char* img_name_;
    SystemString text_;
};



static const std::array<HintInfo, 9> hints = {
    {{"<none>", SystemString::hint_gamespeed},
     {"hint_infirmary", SystemString::hint_infirmary},
     {"<none>", SystemString::hint_navigation},
     {"hint_goblin", SystemString::hint_doors},
     {"<none>", SystemString::hint_hotkeys},
     {"<none>", SystemString::hint_glossary},
     {"hint_plunder", SystemString::hint_plunder},
     {"hint_damaged_core", SystemString::hint_damaged_core},
     {"<none>", SystemString::hint_tutorials}}};



static void show_hint(Platform& pfrm, const HintInfo& info, TextView& text)
{
    if (str_cmp("<none>", info.img_name_) == 0) {
        pfrm.load_overlay_texture("hint_infirmary");
        text.assign(loadstr(pfrm, info.text_)->c_str(), {2, 4}, {26, 18}, 0);
    } else {
        pfrm.load_overlay_texture(info.img_name_);
        draw_image(pfrm, 82, 18, 4, 12, 10, Layer::overlay);
        text.assign(loadstr(pfrm, info.text_)->c_str(), {2, 6}, {16, 12}, 0);
    }
}



void HintScene::enter(Platform& pfrm, App&, Scene& prev)
{
    pfrm.fill_overlay(82);

    show_hint(pfrm, hints[hint_index_], *body_);

    heading_.emplace(pfrm, OverlayCoord{1, 1});
    heading_->assign(
        SYSTR(hint_title)->c_str(),
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
            pfrm.screen().schedule_fade(
                1.f - smoothstep(0.f, fade_duration, timer_),
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
            SYSTR(hint_title)->c_str(),
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
