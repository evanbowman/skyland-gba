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

#include "skyland/metaclassIndex.hpp"
#include "worldScene.hpp"



namespace skyland
{



class UpgradePromptScene : public ActiveWorldScene
{
public:
    UpgradePromptScene(const Vec2<u8>& coord,
                       MetaclassIndex upgrade_from,
                       MetaclassIndex upgrade_to);


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


private:
    MetaclassIndex upgrade_from_;
    MetaclassIndex upgrade_to_;
    Vec2<u8> target_coord_;

    Optional<Text> text_;
    Optional<Text> yes_text_;
    Optional<Text> no_text_;

    Time flicker_timer_ = 0;
    bool flicker_on_ = false;
};



} // namespace skyland
