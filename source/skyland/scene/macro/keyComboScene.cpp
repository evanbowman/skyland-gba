////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
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



ScenePtr KeyComboScene::update(Time delta)
{
    if (APP.player().button_down(Button::start) or
        APP.key_callback_processor().seek_state() ==
            KeyCallbackProcessor::seq_max - 1 or
        (APP.key_callback_processor().possibilities() == 1 and
         APP.key_callback_processor().match())) {

        if (auto binding = APP.key_callback_processor().match()) {
            binding->callback_();
        }
        APP.key_callback_processor().reset();


        return make_scene<SelectorScene>();
    }

    APP.key_callback_processor().update();

    Button found = Button::count;
    for (int i = 0; i < (int)Button::count; ++i) {
        auto k = (Button)i;
        if (PLATFORM.input().down_transition(k)) {
            found = k;
            break;
        }
    }

    if (found not_eq Button::count and found not_eq Button::alt_1) {

        text_data_ += [found] {
            switch (found) {
            case Button::left:
                return "l-";

            case Button::right:
                return "r-";

            case Button::up:
                return "u-";

            case Button::down:
                return "d-";

            case Button::action_1:
                return "a-";

            case Button::action_2:
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
