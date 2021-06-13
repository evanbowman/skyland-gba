#pragma once


#include "skyland/scene.hpp"
#include "graphics/overlay.hpp"



namespace skyland {



class WorldScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


private:
    std::optional<UIMetric> coins_;
};



} // namespace skyland
