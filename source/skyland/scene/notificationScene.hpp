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



class NotificationScene : public ActiveWorldScene
{
public:
    NotificationScene(const StringBuffer<80>& msg, DeferredScene next_scene)
        : next_scene_(next_scene), msg_(msg)
    {
    }


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;
    void exit(Scene& next) override;


public:
    Optional<Text> description_;
    DeferredScene next_scene_;
    StringBuffer<80> msg_;
};



} // namespace skyland
