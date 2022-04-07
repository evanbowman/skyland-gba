////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
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

    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;

    ScenePtr<Scene> update(Platform&, App&, Microseconds) override;


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

    static Factory factory_;
};



} // namespace skyland
