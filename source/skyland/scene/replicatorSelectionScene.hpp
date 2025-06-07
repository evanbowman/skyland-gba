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


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland
{



class ReplicatorSelectionScene : public ActiveWorldScene
{
public:
    ReplicatorSelectionScene(bool near) : near_(near)
    {
    }


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


private:
    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;
    Time exit_countdown_ = 0;
    bool near_ = true;
};



} // namespace skyland
