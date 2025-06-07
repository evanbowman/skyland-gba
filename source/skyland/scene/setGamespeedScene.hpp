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


#include "worldScene.hpp"



namespace skyland
{



class SetGamespeedScene : public WorldScene
{
public:
    ScenePtr update(Time delta) override;

    void enter(Scene&) override;
    void exit(Scene&) override;


    int button_mode_ = 0;


private:
    void repaint_selector();

    Optional<Text> speed_text_;

    int selection_ = 0;
};



} // namespace skyland
