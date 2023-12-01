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


#include "keyComboScene.hpp"
#include "selectorScene.hpp"
#include "skyland/keyCallbackProcessor.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland::macro
{



void KeyComboScene::enter(Scene& prev)
{
    text_data_ = SYSTR(key_combo_prompt)->c_str();

    text_.emplace(text_data_.c_str(), OverlayCoord{0, 19});
    for (int i = text_->len(); i < 30; ++i) {
        text_->append(" ");
    }

    for (int i = 0; i < 30; ++i) {
        PLATFORM.set_tile(Layer::overlay, i, 18, 425);
    }

    APP.key_callback_processor().reset();
}



void KeyComboScene::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);
    text_.reset();
}



ScenePtr<Scene> KeyComboScene::update(Microseconds delta)
{
    if (APP.player().key_down(Key::start) or
        APP.key_callback_processor().seek_state() ==
            KeyCallbackProcessor::seq_max - 1 or
        (APP.key_callback_processor().possibilities() == 1 and
         APP.key_callback_processor().match())) {

        if (auto binding = APP.key_callback_processor().match()) {
            binding->callback_();
        }
        APP.key_callback_processor().reset();


        return scene_pool::alloc<SelectorScene>();
    }

    APP.key_callback_processor().update();

    Key found = Key::count;
    for (int i = 0; i < (int)Key::count; ++i) {
        auto k = (Key)i;
        if (PLATFORM.keyboard().down_transition(k)) {
            found = k;
            break;
        }
    }

    if (found not_eq Key::count and found not_eq Key::alt_1) {

        text_data_ += [found] {
            switch (found) {
            case Key::left:
                return "l-";

            case Key::right:
                return "r-";

            case Key::up:
                return "u-";

            case Key::down:
                return "d-";

            case Key::action_1:
                return "a-";

            case Key::action_2:
                return "b-";

            default:
                return "err";
            }
        }();

        text_->assign(text_data_.c_str());

        for (int i = text_->len(); i < 30; ++i) {
            text_->append(" ");
        }
    }

    return null_scene();
}



} // namespace skyland::macro
