#pragma once



#include "skyland/scene.hpp"
#include "string.hpp"



namespace skyland {



class LoadLevelScene : public Scene {
public:
    LoadLevelScene(const char* script)
    {
        script_name_ = script;
    }


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    StringBuffer<32> script_name_;
};



}
