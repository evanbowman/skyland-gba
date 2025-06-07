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
#include "skyland/scene/module.hpp"



namespace skyland
{



class AchievementViewerModule : public Module<AchievementViewerModule>
{
public:
    static SystemString module_name()
    {
        return SystemString::module_achievements;
    }


    static u16 icon()
    {
        return 1576;
    }


    static bool run_scripts()
    {
        return true;
    }


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    static bool stop_sound()
    {
        return false;
    }


private:
    void load_page(int page);


    Optional<Text> achievements_heading_;
    Optional<Text> count_text_;
    Optional<Text> item_name_;
    Optional<Text> item_details_;
    Optional<TextView> achievement_description_;
    Optional<Text> achievement_name_;
    Optional<Text> unlocks_text_;


    int page_ = 0;


    static Factory factory_;
};



} // namespace skyland
