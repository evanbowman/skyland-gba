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

#include "skyland/achievement.hpp"
#include "worldScene.hpp"



namespace skyland
{



class AchievementNotificationScene : public WorldScene
{
public:
    AchievementNotificationScene(achievements::Achievement achievement,
                                 DeferredScene next_scene,
                                 bool skip_fade = false)
        : achievement_(achievement), next_scene_(next_scene),
          skip_fade_(skip_fade)
    {
    }


    ScenePtr update(Time delta) override;


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


private:
    enum class State {
        fade_in,
        animate_box_sweep,
        wait,
        fade_out,
    } state_ = State::fade_in;


    achievements::Achievement achievement_;

    Optional<Text> achievement_text_;
    Optional<Text> achievement_name_;
    Optional<Text> item_name_;
    Optional<Text> item_details_;

    Optional<Text> unlocked_text_;


    DeferredScene next_scene_;

    bool skip_fade_;


    Time timer_ = 0;
};



} // namespace skyland
