////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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


    void enter(Platform& pfrm, App& app, Scene& prev) override;


    void exit(Platform& pfrm, App& app, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


private:
    MetaclassIndex upgrade_from_;
    MetaclassIndex upgrade_to_;
    Vec2<u8> target_coord_;

    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;

    Microseconds flicker_timer_ = 0;
    bool flicker_on_ = false;
};



} // namespace skyland
