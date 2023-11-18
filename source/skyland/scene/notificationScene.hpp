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


    ScenePtr<Scene> update(App& app, Microseconds delta) override;


    void enter(App& app, Scene& prev) override;
    void exit(App& app, Scene& next) override;


public:
    std::optional<Text> description_;
    DeferredScene next_scene_;
    StringBuffer<80> msg_;
};



} // namespace skyland
