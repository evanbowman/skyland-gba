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

#include "readyScene.hpp"
#include "rewindScene.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "worldScene.hpp"


namespace skyland
{



class EasyModeRewindScene : public WorldScene
{
public:
    void enter(Scene& prev) override
    {
        WorldScene::enter(prev);

        PLATFORM.load_overlay_texture("overlay_challenges");

        yes_text_.emplace(OverlayCoord{3, 7});
        no_text_.emplace(OverlayCoord{3, 9});

        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(no)->c_str());

        auto title_str = SYSTR(easy_mode_auto_rewind_title);
        u8 mg = centered_text_margins(utf8::len(title_str->c_str()));

        title_.emplace(title_str->c_str(), OverlayCoord{mg, 1});

        auto lives = SYSTR(easy_mode_auto_rewind_text);

        text_.emplace(

            format(lives->c_str(), APP.persistent_data().lives_ + 1).c_str(),
            OverlayCoord{1, 5});

        PLATFORM.screen().pixelate(128, false);
        PLATFORM.screen().schedule_fade(0.7f);
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.load_overlay_texture("overlay");
        WorldScene::exit(next);

        title_.reset();
        text_.reset();
        yes_text_.reset();
        no_text_.reset();

        PLATFORM.screen().pixelate(0, false);
        PLATFORM.screen().schedule_fade(0.f);
    }


    ScenePtr<Scene> update(Time delta) override
    {
        if (APP.player().key_down(Key::up)) {
            if (selected_ not_eq 0) {
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 0;
        } else if (APP.player().key_down(Key::down)) {
            if (selected_ not_eq 1) {
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
            selected_ = 1;
        }

        if (APP.player().key_down(Key::action_1)) {
            switch (selected_) {
            case 0:
                state_bit_store(StateBit::easy_mode_rewind_declined, false);
                return scene_pool::alloc<RewindScene>(false);

            case 1:
                state_bit_store(StateBit::easy_mode_rewind_declined, true);
                return scene_pool::alloc<ReadyScene>();
            }
        }



        // NOTE: because the procgen ai forcibly sets level number in top
        // corner.
        PLATFORM.set_tile(Layer::overlay, calc_screen_tiles().x - 1, 0, 0);
        PLATFORM.set_tile(Layer::overlay, calc_screen_tiles().x - 2, 0, 0);

        if (selected_ == 0) {
            PLATFORM.set_tile(Layer::overlay, 1, yes_text_->coord().y, 87);
            PLATFORM.set_tile(Layer::overlay, 1, no_text_->coord().y, 0);
        } else {
            PLATFORM.set_tile(Layer::overlay, 1, yes_text_->coord().y, 0);
            PLATFORM.set_tile(Layer::overlay, 1, no_text_->coord().y, 87);
        }


        return null_scene();
    }

private:
    Optional<Text> title_;
    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
    Optional<Text> lives_;

    int selected_ = 0;
};



} // namespace skyland
