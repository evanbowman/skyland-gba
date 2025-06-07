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



class FadeOutScene : public WorldScene
{
public:
    ScenePtr update(Time delta) override;


    void display() override;


    void exit(Scene& prev) override;
    void enter(Scene& next) override;


private:
    Time timer_ = 0;
    u16 circ_effect_radius_ = 0;
};



} // namespace skyland
