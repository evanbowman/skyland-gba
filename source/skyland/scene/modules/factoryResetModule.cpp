////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "factoryResetModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr FactoryResetModule::update(Time delta)
{
    if (not text_) {
        PLATFORM.screen().fade(0.9f);
        PLATFORM.screen().fade(1.f);
        text_.emplace();
        text_->assign(SYSTR(factory_reset)->c_str(), {1, 1}, {28, 8});
    }

    if (APP.player().key_down(Key::action_2)) {
        text_.reset();
        return make_scene<TitleScreenScene>(3);
    }

    if (APP.player().key_pressed(Key::select) and
        APP.player().key_down(Key::action_1)) {
        ++key_count_;
        if (key_count_ == 5) {
            flash_filesystem::destroy();
            PLATFORM.restart();
        }
    }

    return null_scene();
}



FactoryResetModule::Factory FactoryResetModule::factory_;



} // namespace skyland
