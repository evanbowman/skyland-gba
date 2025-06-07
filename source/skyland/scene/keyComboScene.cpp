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
#include "inspectP2Scene.hpp"
#include "readyScene.hpp"
#include "skyland/keyCallbackProcessor.hpp"
#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



void KeyComboScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    if (not near_) {
        far_camera();
    }

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
    ActiveWorldScene::exit(next);

    PLATFORM.fill_overlay(0);
    text_.reset();
}



ScenePtr KeyComboScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.player().key_down(Key::start) or
        APP.key_callback_processor().seek_state() ==
            KeyCallbackProcessor::seq_max - 1 or
        (APP.key_callback_processor().possibilities() == 1 and
         APP.key_callback_processor().match())) {

        if (auto binding = APP.key_callback_processor().match()) {
            binding->callback_();
        }
        APP.key_callback_processor().reset();

        if (is_far_camera()) {
            return make_scene<InspectP2Scene>();
        } else {
            return make_scene<ReadyScene>();
        }
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



} // namespace skyland
