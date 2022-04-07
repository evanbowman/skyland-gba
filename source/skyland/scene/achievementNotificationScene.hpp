////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    enum class State {
        fade_in,
        animate_box_sweep,
        wait,
        fade_out,
    } state_ = State::fade_in;


    achievements::Achievement achievement_;

    std::optional<Text> achievement_text_;
    std::optional<Text> achievement_name_;
    std::optional<Text> item_name_;
    std::optional<Text> item_details_;

    std::optional<Text> unlocked_text_;


    DeferredScene next_scene_;

    bool skip_fade_;


    Microseconds timer_ = 0;
};



} // namespace skyland
