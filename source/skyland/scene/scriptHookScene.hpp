#pragma once


#include "worldScene.hpp"



namespace skyland {



class ScriptHookScene : public WorldScene {
public:
    ScriptHookScene(const char* invoke_hook_name,
                    DeferredScene next_scene) :
        next_scene_(next_scene),
        invoke_hook_name_(invoke_hook_name)
    {
    }


    ScenePtr<Scene> update(Platform& pfrm,
                           App& app,
                           Microseconds delta) override;


private:
    DeferredScene next_scene_;
    const StringBuffer<32> invoke_hook_name_;
};



void invoke_hook(Platform& pfrm, const char* lisp_hook_name);



}
