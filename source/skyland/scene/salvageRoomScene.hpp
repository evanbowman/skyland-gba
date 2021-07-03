#pragma once


#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "worldScene.hpp"



namespace skyland {



class SalvageRoomScene : public ActiveWorldScene {
public:
    SalvageRoomScene()
    {
    }


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


private:
    std::optional<Text> text_;
    std::optional<Text> yes_text_;
    std::optional<Text> no_text_;
    Microseconds exit_countdown_ = 0;
};


static const Float salvage_factor = 0.65f;



} // namespace skyland
