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
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland
{



class Island;



class SalvageRoomScene : public ActiveWorldScene
{
public:
    SalvageRoomScene(bool near = true) : near_(near)
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    Island* island(App& app);


    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
    Microseconds exit_countdown_ = 0;
    bool near_;
};


static const Float salvage_factor = 0.65f;



} // namespace skyland
