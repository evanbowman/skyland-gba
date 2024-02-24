////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "graphics/overlay.hpp"
#include "skyland/player/player.hpp"
#include "skyland/scene.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class MenuPromptScene : public Scene
{
public:
    using OptCallback = Function<4, void()>;


    MenuPromptScene(SystemString msg,
                    SystemString opt_1,
                    SystemString opt_2,
                    DeferredScene next,
                    OptCallback opt_1_callback,
                    OptCallback opt_2_callback)
        : next_(next), msg_(msg), opt_1_(opt_1), opt_2_(opt_2),
          opt_1_callback_(opt_1_callback), opt_2_callback_(opt_2_callback)
    {
    }


    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    void enter(Scene& prev) override
    {
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);

        text_.emplace();
        text_->assign(loadstr(msg_)->c_str(), {1, 1}, {28, 14}, 0);

        t1_.emplace(OverlayCoord{3, 16});

        t1_->assign(loadstr(opt_1_)->c_str(), sel_colors);

        t2_.emplace(loadstr(opt_2_)->c_str(), OverlayCoord{3, 18});

        PLATFORM.set_tile(Layer::overlay, 1, 16, 475);
        PLATFORM.set_tile(Layer::overlay, 1, 18, 0);

        if (play_alert_sfx_) {
            PLATFORM.speaker().play_sound("click_digital_1", 1);
        }
    }


    void exit(Scene& next) override
    {
        text_.reset();
        t1_.reset();
        t2_.reset();

        PLATFORM.fill_overlay(0);

        if (not skip_unfade_) {
            PLATFORM.screen().schedule_fade(0.f);
        }
    }


    ScenePtr<Scene> update(Time delta)
    {
        if (player().key_down(Key::action_1)) {
            if (cursor_ == 0) {
                opt_1_callback_();
            } else {
                opt_2_callback_();
            }
            PLATFORM.speaker().play_sound("button_wooden", 3);
            return next_();
        }

        if (player().key_down(Key::up)) {
            cursor_ = 0;
            t1_->assign(loadstr(opt_1_)->c_str(), sel_colors);
            t2_->assign(loadstr(opt_2_)->c_str());
            PLATFORM.set_tile(Layer::overlay, 1, 16, 475);
            PLATFORM.set_tile(Layer::overlay, 1, 18, 0);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        if (player().key_down(Key::down)) {
            cursor_ = 1;
            t1_->assign(loadstr(opt_1_)->c_str());
            t2_->assign(loadstr(opt_2_)->c_str(), sel_colors);
            PLATFORM.set_tile(Layer::overlay, 1, 18, 475);
            PLATFORM.set_tile(Layer::overlay, 1, 16, 0);
            PLATFORM.speaker().play_sound("cursor_tick", 0);
        }

        return null_scene();
    }


private:
    DeferredScene next_;
    SystemString msg_;
    SystemString opt_1_;
    SystemString opt_2_;

    Optional<TextView> text_;
    Optional<Text> t1_;
    Optional<Text> t2_;

    int cursor_ = 0;

    OptCallback opt_1_callback_;
    OptCallback opt_2_callback_;

public:
    bool play_alert_sfx_ = true;
    bool skip_unfade_ = false;
};



} // namespace skyland
