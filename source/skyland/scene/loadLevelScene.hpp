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



#include "string.hpp"
#include "worldScene.hpp"



namespace skyland
{



class LoadLevelScene : public ActiveWorldScene
{
public:
    LoadLevelScene()
    {
    }


    void enter(Scene& prev) override;


    void exit(Scene& next) override;



    ScenePtr update(Time delta) override;
};



} // namespace skyland
