#pragma once


#include "graphics/overlay.hpp"
#include "skyland/coins.hpp"
#include "skyland/power.hpp"
#include "skyland/scene.hpp"



namespace skyland {



class WorldScene : public Scene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;


    void display(Platform&, App&) override;


    void enter(Platform&, App&, Scene& prev) override;


    void exit(Platform&, App&, Scene& next) override;


    void persist_ui();


    void far_camera();


protected:
    std::optional<UIMetric> coins_;
    std::optional<UIMetric> power_;

private:
    void set_pause_icon(Platform& pfrm, bool paused);


    bool persistent_ui_ = false;
    Microseconds coin_hide_timer_ = 0;
    Microseconds camera_update_timer_ = 0;
    Microseconds power_hide_timer_ = 0;
    Coins last_coins_ = 0;
    Power last_power_supplied_ = 0;
    Power last_power_used_ = 0;
    bool far_camera_ = false;
};



class ActiveWorldScene : public WorldScene {
public:
    ScenePtr<Scene> update(Platform&, App&, Microseconds delta) override;
};



} // namespace skyland
