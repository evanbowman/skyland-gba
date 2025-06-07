////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "scene/startAdventureScene.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class InitScene : public Scene
{
public:
    ScenePtr update(Time delta) override
    {


        return null_scene();
    }
};



} // namespace skyland
