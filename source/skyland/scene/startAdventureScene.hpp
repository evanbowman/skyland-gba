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


#include "skyland/scene.hpp"



namespace skyland
{



class StartAdventureScene : public Scene
{
public:
    void enter(Scene& prev) override;


    ScenePtr update(Time delta) override;


    void reset_state();


    bool loaded_ = false;
    int continue_opt_sel_ = 0;
};



} // namespace skyland
