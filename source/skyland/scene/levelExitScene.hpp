////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "scriptHookScene.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



template <typename NextScene> class LevelExitScene : public ScriptHookScene
{
public:
    template <typename... Args>
    LevelExitScene(Args&&... args)
        : ScriptHookScene(
              "on-level-exit",
              make_deferred_scene<NextScene>(std::forward<Args>(args)...))
    {
    }
};



} // namespace skyland
