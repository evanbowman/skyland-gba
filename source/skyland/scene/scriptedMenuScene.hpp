#pragma once

#include "worldScene.hpp"



namespace skyland
{



class ScriptedMenuScene : public ActiveWorldScene
{
public:
    ScriptedMenuScene(const char* script_name);


    void enter(App&, Scene& prev) override;
    void exit(App&, Scene& next) override;


    ScenePtr<Scene> update(App&, Microseconds delta) override;


    void display(App&) override;


private:
    StringBuffer<32> menu_name_;
};



} // namespace skyland
