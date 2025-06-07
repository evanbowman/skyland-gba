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


    ScenePtr update(Time delta) override;


private:
    DeferredScene next_scene_;
    const StringBuffer<32> invoke_hook_name_;
};



struct InvokeHookConfig
{
    bool reset_timestream_ = true;
};



void invoke_hook(const char* lisp_hook_name,
                 const InvokeHookConfig& config = {});



} // namespace skyland
