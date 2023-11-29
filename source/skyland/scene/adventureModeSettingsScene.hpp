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

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class AdventureModeSettingsScene : public Scene
{
public:
    AdventureModeSettingsScene(bool newgame = false) : newgame_(newgame)
    {
    }

    void enter(Scene& prev) override;


    void exit(Scene& prev) override;


    ScenePtr<Scene> update(Microseconds delta) override;


private:
    std::optional<Text> difficulty_text_;
    std::optional<Text> easy_text_;
    std::optional<Text> normal_text_;
    std::optional<Text> hard_text_;
    bool newgame_;
    u8 original_;
};



} // namespace skyland
