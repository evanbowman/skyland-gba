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



class ScriptHookScene : public WorldScene
{
public:
    ScriptHookScene(const char* invoke_hook_name, DeferredScene next_scene)
        : next_scene_(next_scene), invoke_hook_name_(invoke_hook_name)
    {
    }


    ScenePtr<Scene> update(App& app, Microseconds delta) override;


private:
    DeferredScene next_scene_;
    const StringBuffer<32> invoke_hook_name_;
};



struct InvokeHookConfig
{
    bool reset_timestream_ = true;
};



void invoke_hook(App& app,
                 const char* lisp_hook_name,
                 const InvokeHookConfig& config = {});



} // namespace skyland
