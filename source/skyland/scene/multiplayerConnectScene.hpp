#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class MultiplayerConnectScene : public Scene
{
public:
    void enter(Platform&, App&, Scene& prev) override;
    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    bool ready_ = false;
    std::optional<Text> text_;
    Microseconds timer_ = 0;
};



} // namespace skyland
