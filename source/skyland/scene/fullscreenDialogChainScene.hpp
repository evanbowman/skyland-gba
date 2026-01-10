////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "skyland/scene.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"
#include "boxedDialogScene.hpp"



namespace skyland
{



class FullscreenDialogChainScene : public Scene
{
public:

    void enter(Scene& prev) override;


    ScenePtr update(Time delta) override;

};



}
