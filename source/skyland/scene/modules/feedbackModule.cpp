////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "feedbackModule.hpp"
#include "skyland/scene/qrViewerScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"



namespace skyland
{



ScenePtr FeedbackModule::update(Time delta)
{
    return make_scene<ConfiguredURLQRViewerScene>(
        "/scripts/config/feedback.lisp", "", "", [] {
            PLATFORM.screen().schedule_fade(1);
            PLATFORM.screen().clear();
            PLATFORM.screen().display();
            return make_scene<TitleScreenScene>(3);
        });
}



FeedbackModule::Factory FeedbackModule::factory_(false);



} // namespace skyland
