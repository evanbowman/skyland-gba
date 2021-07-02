#pragma once

#include "skyland/scene.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class HighscoresScene : public Scene {
public:
    void enter(Platform& pfrm, App& app, Scene& prev) override;
    void exit(Platform& pfrm, App& app, Scene& next) override;

    ScenePtr<Scene> update(Platform&, App&, Microseconds) override;


private:
    Buffer<Text, 8> lines_;
};



}
