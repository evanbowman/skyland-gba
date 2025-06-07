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



class HighscoresScene : public Module<HighscoresScene>
{
public:
    HighscoresScene();


    HighscoresScene(bool show_current_score, int title_screen_page);

    void enter(Scene& prev) override;
    void exit(Scene& next) override;

    ScenePtr update(Time) override;


    static SystemString module_name()
    {
        return SystemString::module_highscores;
    }


    static u16 icon()
    {
        return 1656;
    }


    static bool run_scripts()
    {
        return false;
    }


private:
    Buffer<Text, 8> lines_;
    bool show_current_score_;
    bool disable_writeback_;
    int title_screen_page_;

    Optional<Text> upload_hint_;
    Optional<Text> leaderboard_text_;

    static Factory factory_;
};



} // namespace skyland
